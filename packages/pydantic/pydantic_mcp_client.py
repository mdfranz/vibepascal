import asyncio
import contextlib
import dataclasses
import logging
import os
import sys
import time
import json
from pydantic import TypeAdapter

import logfire
from dotenv import load_dotenv
from mcp.shared.exceptions import McpError
from vibepascal_shared.guidance_loader import format_guidance_block, load_guidance
from vibepascal_shared.llm_observability import (
    game_console_enabled,
    print_game,
    setup_logger,
)
from pydantic_ai import Agent, ModelSettings
from pydantic_ai.exceptions import ModelRetry, UnexpectedModelBehavior, UsageLimitExceeded
from pydantic_ai.usage import UsageLimits
from pydantic_ai.capabilities import Hooks, Thinking, ProcessHistory
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
# 2.31.0 renamed the `google-gla` provider prefix to plain `google` (the old prefix now raises
# "Unknown provider" from `infer_model`) - caught while testing the pydantic-ai 2.31.0 bump.
DEFAULT_MODEL: KnownModelName = "google:gemini-3.7-flash"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/pydantic_mcp_client-{EPOCH}.log"

logger = setup_logger(__name__, LOG_FILE)

# Logfire replaces the old log_kv-based provider_call/tool_call/run_summary telemetry for
# this client. Off by default; opt in with LOGFIRE_ENABLED=1 (see packages/shared/OBSERVABILITY.md).
LOGFIRE_ENABLED = os.environ.get("LOGFIRE_ENABLED", "0") not in {"0", "false", "False"}
if LOGFIRE_ENABLED:
    # data_dir defaults to a cwd-relative ".logfire/", but this client is invoked from
    # different working directories (e.g. repo root via pydantic-mcp-game.sh) - pin it to
    # ~/.logfire/ (same place `logfire auth`/`logfire projects use` write to) so project
    # credentials resolve consistently regardless of cwd.
    logfire.configure(
        service_name="pydantic-mcp-client",
        environment=os.environ.get("LOGFIRE_ENVIRONMENT", "development"),
        data_dir=os.path.expanduser("~/.logfire"),
    )
    logfire.instrument_pydantic_ai()
    logfire.instrument_mcp()


# --- GPT-OSS tool-call hardening -----------------------------------------------------------
#
# GPT-OSS (and other Harmony-format models served through some OpenAI-compatible endpoints)
# occasionally emits a tool call whose name carries a trailing `<|channel|>commentary` marker
# from its Harmony response format instead of the plain registered tool name. Left alone this
# burns through the MCP toolset's retry budget and can end the run.
#
# The repair has to happen on the raw `ModelResponse`, in `after_model_request` - not inside
# `MCPToolset.process_tool_call` (the file's existing `McpError` bridge below, `process_tool_call`
# in `run_pydantic_agent`). By the time a tool call reaches `process_tool_call`,
# `ToolManager._resolve_tool` has already looked the name up against the registered tool set and
# raised `ModelRetry('Unknown tool name...')` for anything that doesn't match - a malformed
# Harmony name never survives that lookup, so `process_tool_call` never sees it.
# `after_model_request` fires strictly before tool resolution, so it's the only point that can
# fix the name before the framework gives up on it.
HARMONY_TOOL_NAME_SUFFIX = "<|channel|>commentary"

_tool_name_hooks = Hooks()


@_tool_name_hooks.on.after_model_request
async def _repair_harmony_tool_names(ctx, *, request_context, response):
    known_tool_names = {td.name for td in request_context.model_request_parameters.function_tools}
    repaired_parts = None
    for i, part in enumerate(response.parts):
        if not isinstance(part, ToolCallPart) or not part.tool_name.endswith(HARMONY_TOOL_NAME_SUFFIX):
            continue
        repaired_name = part.tool_name[: -len(HARMONY_TOOL_NAME_SUFFIX)]
        if repaired_name not in known_tool_names:
            continue  # Only repair names that resolve to a real, registered tool - never guess.
        if repaired_parts is None:
            repaired_parts = list(response.parts)
        repaired_parts[i] = dataclasses.replace(part, tool_name=repaired_name)
        logger.info(f"Repaired malformed Harmony tool name: {part.tool_name!r} -> {repaired_name!r}")
        if LOGFIRE_ENABLED:
            logfire.info(
                "harmony_tool_name_repaired original={original} repaired={repaired}",
                original=part.tool_name,
                repaired=repaired_name,
            )
    if repaired_parts is None:
        return response  # No malformed names in this response - identical to an unpatched run.
    return dataclasses.replace(response, parts=repaired_parts)


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
    capabilities.append(_tool_name_hooks)

    # MCPToolset's built-in tool_error_behavior="retry" only converts fastmcp.exceptions.ToolError
    # into a ModelRetry (so the model can self-correct instead of crashing the run). It does NOT
    # cover mcp.shared.exceptions.McpError - a protocol-level "invalid params" rejection raised when
    # a model sends args that fail the tool's JSON schema (e.g. a weaker model passing seed="None"
    # instead of an integer/null). Without this, that error propagates uncaught and kills the whole
    # game run. Bridge it the same way tool_error_behavior already does for ToolError.
    async def process_tool_call(ctx, call_tool, name, tool_args):
        try:
            return await call_tool(name, tool_args)
        except McpError as e:
            raise ModelRetry(message=str(e)) from e

    server = MCPToolset(MCP_URL, max_retries=3, process_tool_call=process_tool_call)

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
        retries=3,
        model_settings=ModelSettings(
            # 4096 was too tight for verbose/reasoning-heavy models (e.g. Kimi-k3 hit this on a
            # trailing request right after a game turn, raising UnexpectedModelBehavior before any
            # usable output was produced - see logfire_results/openrouter-deepseek-vs-gemini-2026-08-06.md).
            max_tokens=8192,
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

    # This agent is a tool-using game player, not a chat agent. Pydantic AI otherwise
    # treats plain text as a valid final answer, which lets a model return e.g. "TAKE MAP"
    # after LOOK without ever executing that action through MCP.
    latest_game_state = {"is_playing": True}

    @agent.output_validator
    def require_tool_action_while_playing(output: str) -> str:
        if latest_game_state.get("is_playing", True):
            raise ModelRetry(
                "The game is still active. Do not return a plain-text command; call an MCP game tool."
            )
        return output

    prompt = (
        f"Start by calling the 'command' tool with command='LOOK' and reset=True. "
        f"Then continue playing 'Echoes of Dustwood' for up to {max_turns} turns to increase your score."
    )

    processed_parts = set()

    agent_run = None
    run_span = (
        logfire.span(
            "pydantic_game_run",
            model=model_name,
            level=level,
            max_turns=max_turns,
            session_id=active_session_id,
        )
        if LOGFIRE_ENABLED
        else contextlib.nullcontext()
    )
    with run_span:
        try:
            async with agent.iter(
                prompt,
                message_history=loaded_messages,
                usage_limits=UsageLimits(request_limit=max_turns * 4)
            ) as run_iter:
                agent_run = run_iter
                async for node in agent_run:
                    # Per-model-call token usage and per-tool-call args/results are captured
                    # automatically by logfire.instrument_pydantic_ai() when LOGFIRE_ENABLED=1 -
                    # no manual tracking needed here.

                    # Process messages in this yield
                    for msg in agent_run.all_messages():
                        if not hasattr(msg, "parts"):
                            continue

                        for part in msg.parts:
                            part_id = id(part)
                            if part_id in processed_parts:
                                continue

                            if isinstance(part, ThinkingPart):
                                processed_parts.add(part_id)
                            elif isinstance(part, TextPart):
                                processed_parts.add(part_id)
                            elif isinstance(part, ToolCallPart):
                                if delay > 0 and part.tool_name != "look":
                                    await asyncio.sleep(delay)
                                processed_parts.add(part_id)
                            elif isinstance(part, ToolReturnPart):
                                content = part.content
                                processed_parts.add(part_id)

                                if isinstance(content, dict):
                                    output = content.get("output", "")
                                    state = content.get("state")
                                    if not output and "structuredContent" in content:
                                        sc = content["structuredContent"]
                                        output = sc.get("output", "")
                                        state = sc.get("state")

                                    if isinstance(state, dict):
                                        latest_game_state.clear()
                                        latest_game_state.update(state)

                                        if output:
                                            turns = state.get("turns", 0)
                                            room = state.get("room_name") or state.get("roomName") or "Unknown"
                                            score = state.get("score", 0)
                                            thirst = state.get("thirst", 0)

                                            if game_console_enabled():
                                                print_game(f"\n[turn={turns} room={room} score={score} thirst={thirst}]\n{output.strip()}\n")

                                            if LOGFIRE_ENABLED:
                                                logfire.info(
                                                    "game_turn {turn} room={room} score={score} thirst={thirst}",
                                                    turn=turns,
                                                    room=room,
                                                    score=score,
                                                    thirst=thirst,
                                                )

                                        if not state.get("is_playing", True):
                                            logger.info("Game ended. Stopping agent.")
                                            raise UsageLimitExceeded("Game ended")

                                        if state.get("turns", 0) >= max_turns:
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
        except Exception as e:
            # Defense in depth: any other error (MCP transport issues, provider outages, etc.)
            # should end this run's game loop gracefully - with a run_summary still logged -
            # rather than crashing the whole script and losing benchmark output for a serial
            # multi-model run. Caught here (rather than left to propagate out of `with run_span`)
            # so it doesn't also take down callers running several models back to back; still
            # reported to Logfire explicitly so it's not silently swallowed.
            logger.error(f"[GAME ENDED - UNEXPECTED ERROR] {type(e).__name__}: {e}")
            if LOGFIRE_ENABLED:
                logfire.exception("game_run_failed model={model}", model=model_name)

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

                if LOGFIRE_ENABLED:
                    logfire.info(
                        "run_summary model={model} input_tokens={input_tokens} output_tokens={output_tokens} "
                        "total_tokens={total_tokens} requests={requests}",
                        model=model_name,
                        input_tokens=usage.input_tokens,
                        output_tokens=usage.output_tokens,
                        total_tokens=usage.total_tokens,
                        cache_read_tokens=usage.cache_read_tokens or None,
                        requests=usage.requests,
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
