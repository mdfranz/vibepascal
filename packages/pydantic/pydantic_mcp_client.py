import asyncio
import logging
import os
import sys
import time
import json
from pydantic import TypeAdapter

from dotenv import load_dotenv
from vibepascal_shared.guidance_loader import format_guidance_block, load_guidance
from vibepascal_shared.llm_observability import (
    Timer,
    format_payload,
    game_console_enabled,
    log_kv,
    print_game,
    provider_payload_logging_enabled,
    setup_logger,
)
from pydantic_ai import Agent, ModelSettings
from pydantic_ai.exceptions import UnexpectedModelBehavior, UsageLimitExceeded
from pydantic_ai.usage import UsageLimits
from pydantic_ai.capabilities import Thinking, ProcessHistory
from pydantic_ai.mcp import MCPToolset
from pydantic_ai.messages import (
    ModelResponse,
    ModelRequest,
    TextPart, 
    ThinkingPart, 
    ToolCallPart, 
    ToolReturnPart,
    ModelMessage,
    UserPromptPart,
    SystemPromptPart
)
from pydantic_ai.models import KnownModelName

# Load environment variables
load_dotenv()

# --- Configuration ---
MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-3-flash-preview"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/pydantic_mcp_client-{EPOCH}.log"

logger = setup_logger(__name__, LOG_FILE)


async def run_pydantic_agent(
    level: str,
    model_name: str,
    delay: int,
    max_turns: int,
    summarize: bool = False,
    windowing: bool = False,
    window_size: int = 6,
    session_id: Optional[str] = None,
):
    logger.info(f"--- Pydantic AI MCP Agent Starting (Model: {model_name}) ---")

    guidance_cfg = load_guidance(level)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")
    guidance_block = format_guidance_block(guidance_cfg)

    reasoning_enabled = os.environ.get("AI_REASONING", "0") not in {"0", "false", "False"}
    capabilities = [Thinking()] if reasoning_enabled else []

    server = MCPToolset(MCP_URL, max_retries=3)

    # 1. Sliding Window History Processor
    def trim_history(messages: list[ModelMessage]) -> list[ModelMessage]:
        limit = window_size * 4
        if len(messages) <= limit:
            return messages

        trim_index = len(messages) - limit
        while trim_index < len(messages):
            msg = messages[trim_index]
            if isinstance(msg, ModelRequest):
                if any(isinstance(part, ToolReturnPart) for part in msg.parts):
                    trim_index -= 1
                    continue
            break

        trim_index = max(0, trim_index)

        system_messages = []
        if messages and isinstance(messages[0], ModelRequest):
            sys_parts = [part for part in messages[0].parts if isinstance(part, SystemPromptPart)]
            if sys_parts:
                system_messages.append(ModelRequest(parts=sys_parts))

        return system_messages + messages[trim_index:]

    if windowing and not summarize:
        capabilities.append(ProcessHistory(trim_history))

    # 2. History Summarizer Processor
    if summarize:
        async def summarize_history(messages: list[ModelMessage]) -> list[ModelMessage]:
            threshold = window_size * 4
            if len(messages) <= threshold:
                return messages

            split_index = len(messages) - (window_size * 4)
            while split_index > 0:
                msg = messages[split_index]
                if isinstance(msg, ModelRequest):
                    if any(isinstance(part, ToolReturnPart) for part in msg.parts):
                        split_index -= 1
                        continue
                break

            split_index = max(1, split_index)
            messages_to_summarize = messages[:split_index]
            recent_messages = messages[split_index:]

            text_parts = []
            for msg in messages_to_summarize:
                if isinstance(msg, ModelRequest):
                    for part in msg.parts:
                        if isinstance(part, UserPromptPart) and part.content:
                            text_parts.append(f"User: {part.content}")
                        elif isinstance(part, ToolReturnPart) and part.content:
                            text_parts.append(f"Tool Result: {part.content}")
                elif isinstance(msg, ModelResponse):
                    for part in msg.parts:
                        if isinstance(part, TextPart) and part.content:
                            text_parts.append(f"Agent: {part.content}")
                        elif isinstance(part, ToolCallPart) and part.args:
                            text_parts.append(f"Agent Tool Call: {part.name} args={part.args}")

            history_text = "\n".join(text_parts)
            if not history_text.strip():
                return messages

            try:
                logger.info(f"Summarizing {len(messages_to_summarize)} historical messages...")
                summarizer_agent = Agent(
                    model=model_name,
                    system_prompt=(
                        "You are a memory compression assistant. "
                        "Summarize the conversation history concisely in bullet points, "
                        "retaining all inventory items, room names, and game turn count."
                    )
                )
                res = await summarizer_agent.run(f"Summarize this game history:\n\n{history_text}")
                summary = res.data

                summary_msg = ModelRequest(parts=[UserPromptPart(f"Summary of past game turns:\n{summary}")])

                system_messages = []
                if messages and isinstance(messages[0], ModelRequest):
                    sys_parts = [part for part in messages[0].parts if isinstance(part, SystemPromptPart)]
                    if sys_parts:
                        system_messages.append(ModelRequest(parts=sys_parts))

                return system_messages + [summary_msg] + recent_messages
            except Exception as ex:
                logger.warning(f"History summarization failed: {ex}")
                return messages

        capabilities.append(ProcessHistory(summarize_history))

    # 3. Session Persistence
    active_session_id = session_id if session_id else f"pydantic-session-{EPOCH}"
    loaded_messages = None
    if session_id:
        filepath = f"sessions/pydantic_sessions/{active_session_id}.json"
        if os.path.exists(filepath):
            try:
                ta = TypeAdapter(list[ModelMessage])
                with open(filepath, "r") as f:
                    json_data = f.read()
                loaded_messages = ta.validate_json(json_data)
                if windowing:
                    loaded_messages = trim_history(loaded_messages)
                logger.info(f"Loaded {len(loaded_messages)} messages from session {active_session_id}")
            except Exception as e:
                logger.warning(f"Failed to load session snapshot: {e}")

    agent = Agent(
        model=model_name,
        toolsets=[server],
        capabilities=capabilities,
        model_settings=ModelSettings(
            max_tokens=4096,
            **({"anthropic_thinking": {"type": "enabled", "budget_tokens": 2048}} if model_name.startswith("anthropic:") else {}),
        ),
        system_prompt=(
            "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
            "Use the available MCP tools to play the game.\n"
            "Start with LOOK to see your surroundings.\n"
            "LOOK does not consume a game turn; do not repeat LOOK if turns did not change.\n"
            "Exits may not be listed. If unsure, try a cardinal move (NORTH/EAST/SOUTH/WEST).\n"
            "Prefer standard parser commands like LOOK, INVENTORY, N/S/E/W, TAKE <item>, USE <item>.\n"
            f"Your goal is to survive, explore, and increase your score. Stop after {max_turns} game turns or when the game ends."
            f"{guidance_block}"
        ),
    )

    prompt = (
        f"Start by calling the 'command' tool with command='LOOK' and reset=True. "
        f"Then continue playing 'Echoes of Dustwood' for up to {max_turns} turns to increase your score."
    )

    processed_parts = set()
    
    # Incremental usage tracking
    last_input_tokens = 0
    last_output_tokens = 0
    last_cache_read_tokens = 0

    # Start the first turn timer
    turn_timer = Timer.start_new()
    
    agent_run = None
    try:
        async with agent.iter(
            prompt,
            message_history=loaded_messages,
            usage_limits=UsageLimits(request_limit=max_turns * 4)
        ) as run_iter:
            agent_run = run_iter
            async for node in agent_run:
                # 1. Capture incremental usage and log provider call for this turn
                current_usage = agent_run.usage
                in_tokens = current_usage.input_tokens or 0
                out_tokens = current_usage.output_tokens or 0
                cache_read = current_usage.cache_read_tokens or 0

                # If usage changed, it means a model call just finished
                if in_tokens > last_input_tokens or out_tokens > last_output_tokens:
                    delta_in = in_tokens - last_input_tokens
                    delta_out = out_tokens - last_output_tokens
                    delta_cache_read = cache_read - last_cache_read_tokens
                    latency = turn_timer.elapsed_ms()

                    log_kv(
                        logger,
                        event="provider_call",
                        client="pydantic_ai",
                        model=model_name,
                        latency_ms=latency,
                        input_tokens=delta_in,
                        output_tokens=delta_out,
                        total_tokens=delta_in + delta_out,
                        cache_read_tokens=delta_cache_read or None,
                        token_scope="call_delta",
                    )

                    # Reset turn timer and usage trackers for the next step
                    last_input_tokens = in_tokens
                    last_output_tokens = out_tokens
                    last_cache_read_tokens = cache_read
                    turn_timer = Timer.start_new()

                # 2. Process messages in this yield
                for msg in agent_run.all_messages():
                    if not hasattr(msg, "parts"):
                        continue
                        
                    for part in msg.parts:
                        part_id = id(part)
                        if part_id in processed_parts:
                            continue
                        
                        if isinstance(part, ThinkingPart):
                            if part.content.strip():
                                logger.info(f"THINKING: {part.content.strip()}")
                            processed_parts.add(part_id)
                        elif isinstance(part, TextPart):
                            if part.content.strip():
                                logger.info(f"AI: {part.content.strip()}")
                            processed_parts.add(part_id)
                        elif isinstance(part, ToolCallPart):
                            args = part.args if isinstance(part.args, str) else str(part.args)
                            logger.info(f"TOOL: {part.tool_name}({args})")
                            if delay > 0 and part.tool_name != "look":
                                await asyncio.sleep(delay)
                            processed_parts.add(part_id)
                        elif isinstance(part, ToolReturnPart):
                            tool_name = part.tool_name
                            content = part.content
                            processed_parts.add(part_id)
                            
                            log_kv(
                                logger,
                                event="tool_call",
                                client="pydantic_ai",
                                tool_name=tool_name,
                                result=(
                                    format_payload(content)
                                    if provider_payload_logging_enabled()
                                    else None
                                ),
                            )
                            
                            if isinstance(content, dict):
                                output = content.get("output", "")
                                state = content.get("state")
                                if not output and "structuredContent" in content:
                                    sc = content["structuredContent"]
                                    output = sc.get("output", "")
                                    state = sc.get("state")
                                
                                if output and isinstance(state, dict):
                                    turns = state.get("turns", 0)
                                    room = state.get("room_name") or state.get("roomName") or "Unknown"
                                    score = state.get("score", 0)
                                    thirst = state.get("thirst", 0)
                                    
                                    if game_console_enabled():
                                        print_game(f"\n[turn={turns} room={room} score={score} thirst={thirst}]\n{output.strip()}\n")
                                    
                                    if turns >= max_turns:
                                        logger.info(f"Turn limit ({max_turns}) reached. Stopping agent.")
                                        raise UsageLimitExceeded(f"Turn limit {max_turns} reached.")

                # Always save session snapshots for each run
                try:
                    os.makedirs("sessions/pydantic_sessions", exist_ok=True)
                    ta = TypeAdapter(list[ModelMessage])
                    serialized = ta.dump_json(agent_run.all_messages())
                    with open(f"sessions/pydantic_sessions/{active_session_id}.json", "wb") as f:
                        f.write(serialized)
                except Exception as e:
                    logger.warning(f"Failed to save session snapshot: {e}")

    except (UnexpectedModelBehavior, UsageLimitExceeded) as e:
        logger.info(f"[GAME ENDED] {e}")

    # Final summary log
    if agent_run is not None:
        try:
            usage = agent_run.usage
            output = ""
            if hasattr(agent_run, "result") and agent_run.result is not None:
                output = getattr(agent_run.result, "output", "")
                usage = getattr(agent_run.result, "usage", usage)
                
            if output:
                logger.info(f"\n[FINAL AGENT RESPONSE]\n{output}")
            
            log_kv(
                logger,
                event="run_summary",
                client="pydantic_ai",
                model=model_name,
                input_tokens=usage.input_tokens,
                output_tokens=usage.output_tokens,
                total_tokens=usage.total_tokens,
                cache_read_tokens=usage.cache_read_tokens or None,
                requests=usage.requests,
                token_scope="run_total",
            )
        except Exception as ex:
            logger.debug(f"Failed to log run summary: {ex}")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("level", nargs="?", default="full")
    parser.add_argument("model", nargs="?", default=DEFAULT_MODEL)
    parser.add_argument("delay", nargs="?", type=int, default=TURN_DELAY)
    parser.add_argument("max_turns", nargs="?", type=int, default=MAX_TURNS)
    parser.add_argument("--summarize", "-s", action="store_true", help="Enable summarization")
    parser.add_argument("--windowing", "-w", action="store_true", help="Enable sliding window history (disabled by default)")
    parser.add_argument("--window-size", "-n", type=int, default=6, help="Window size in game turns (default: 6)")
    parser.add_argument("--session-id", type=str, default=None, help="Session ID to restore or create")

    args = parser.parse_args()

    asyncio.run(
        run_pydantic_agent(
            level=args.level,
            model_name=args.model,
            delay=args.delay,
            max_turns=args.max_turns,
            summarize=args.summarize,
            windowing=args.windowing,
            window_size=args.window_size,
            session_id=args.session_id,
        )
    )
