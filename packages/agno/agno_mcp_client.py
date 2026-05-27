import asyncio
import logging
import os
import sys
import time
from typing import List, Optional

from agno.agent import Agent
from agno.models.anthropic import Claude
from agno.models.google import Gemini
from agno.models.ollama import Ollama
from agno.models.openai import OpenAIChat
from agno.tools.mcp import MCPTools
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
from vibepascal_shared.mcp_command_policy import CommandPolicy, sanitize_command
from pydantic import BaseModel

# Load environment variables
load_dotenv()

# Support GOOGLE_API_KEY for LiteLLM/Gemini
if "GOOGLE_API_KEY" in os.environ and "GEMINI_API_KEY" not in os.environ:
    os.environ["GEMINI_API_KEY"] = os.environ["GOOGLE_API_KEY"]

# --- Configuration ---
MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
MCP_TRANSPORT = os.environ.get("MCP_TRANSPORT", "streamable-http")
DEFAULT_MODEL = "gpt-5-mini"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/agno_mcp_client-{EPOCH}.log"

logger = setup_logger(__name__, LOG_FILE)
logging.getLogger("anyio").setLevel(logging.WARNING)
logging.getLogger("mcp").setLevel(logging.WARNING)

# Global variables for delay
global_delay = TURN_DELAY

# --- State Models ---


class GameSummary(BaseModel):
    room_id: int
    room_name: str
    turns: int
    score: int
    is_playing: bool
    is_riding: bool
    is_dark: bool
    thirst: int
    horse_thirst: int
    has_water: bool
    lamp_lit: bool
    horse_saddled: bool
    inventory: Optional[List[str]] = None


class CommandOutput(BaseModel):
    output: str
    state: GameSummary


def _trim_output(text: str, max_chars: int = 1200) -> str:
    s = (text or "").strip()
    if len(s) <= max_chars:
        return s
    return s[-max_chars:]


def _format_command_result(*, structured_content: dict) -> str:
    result = CommandOutput(**structured_content)
    state = result.state
    logger.info(f"Game output received (State: {state.room_name})")
    if game_console_enabled():
        print_game(
            f"\n[turn={state.turns} room={state.room_name} score={state.score} thirst={state.thirst}\n"
            f"{result.output.strip()}\n"
        )
    return (
        f"--- Game State ---\n"
        f"Room: {state.room_name} (ID: {state.room_id})\n"
        f"Turns: {state.turns}, Score: {state.score}, Playing: {state.is_playing}, Thirst: {state.thirst}\n"
        f"Inventory: {', '.join(state.inventory) if state.inventory else 'Empty'}\n"
        f"Status: Riding={state.is_riding}, Saddled={state.horse_saddled}, Water={state.has_water}\n"
        f"-----------------\n\n"
        f"{result.output}"
    )


async def run_agno_mcp_agent(level: str, model_name: str, delay: int, max_turns: int):
    logger.info(f"--- Agno MCP Client Starting (Model: {model_name}) ---")

    run_timer = Timer.start_new()
    token_accumulator = {
        "input_tokens": 0,
        "output_tokens": 0,
        "total_tokens": 0,
        "reasoning_tokens": 0,
        "requests": 0,
    }

    guidance_cfg = load_guidance(level)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")

    global global_delay
    global_delay = delay

    policy = CommandPolicy.from_env()
    max_llm_calls = max(1, int(max_turns) * int(policy.max_llm_calls_multiplier))
    llm_calls = 0
    history: list[str] = []

    mcp_tools = None
    try:
        # Agno MCP integration
        mcp_tools = MCPTools(url=MCP_URL, transport=MCP_TRANSPORT)
        await mcp_tools.__aenter__()
        last_state: Optional[GameSummary] = None
        last_output_text: str = ""

        async def command(command: str, reset: bool = False) -> str:
            """Send a game command via MCP and return narrative output plus a structured state summary."""
            nonlocal last_state
            nonlocal last_output_text
            logger.info(f"Agent executing command: {command}")
            if game_console_enabled():
                print_game(f"\n> {command}")

            if global_delay > 0 and not reset:
                await asyncio.sleep(global_delay)

            tool_timer = Timer.start_new()
            tool_args = {"command": command, "reset": reset}
            try:
                result = await mcp_tools.session.call_tool(  # type: ignore[union-attr]
                    "command",
                    tool_args,
                )
                log_kv(
                    logger,
                    event="tool_call",
                    client="agno",
                    tool_name="mcp.command",
                    latency_ms=tool_timer.elapsed_ms(),
                    args=(
                        format_payload(tool_args)
                        if provider_payload_logging_enabled()
                        else None
                    ),
                    result=(
                        format_payload(
                            getattr(result, "structuredContent", None)
                            or getattr(result, "content", None)
                        )
                        if provider_payload_logging_enabled()
                        else None
                    ),
                    is_error=getattr(result, "isError", None),
                )
            except Exception as e:
                log_kv(
                    logger,
                    level="error",
                    event="tool_call",
                    client="agno",
                    tool_name="mcp.command",
                    latency_ms=tool_timer.elapsed_ms(),
                    args=(
                        format_payload(tool_args)
                        if provider_payload_logging_enabled()
                        else None
                    ),
                    error=str(e),
                )
                raise

            if result.isError:
                raise RuntimeError("MCP tool 'command' returned an error.")

            if result.structuredContent:
                parsed = CommandOutput(**result.structuredContent)
                last_state = parsed.state
                last_output_text = parsed.output
                return _format_command_result(
                    structured_content=result.structuredContent
                )

            text_parts = [getattr(c, "text", "") for c in (result.content or [])]
            last_output_text = next((t for t in text_parts if t), "No output")
            return last_output_text

        # Initial look + reset
        initial_summary = await command("LOOK", reset=True)
        logger.info(f"\n[STARTING GAME]\n{initial_summary}")
        if last_state is not None:
            policy.observe(
                command="LOOK", state=last_state, output_text=last_output_text
            )

        # Instantiate the model
        if "claude" in model_name.lower():
            clean_model = model_name
            for prefix in ["anthropic:", "anthropic/", "claude:", "claude/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix):]
            model = Claude(id=clean_model)
        elif "gemini" in model_name.lower():
            clean_model = model_name
            for prefix in ["gemini:", "gemini/", "google:", "google/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix):]
            model = Gemini(id=clean_model)
        elif "ollama" in model_name.lower():
            clean_model = model_name
            for prefix in ["ollama:", "ollama/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix) :]
            model = Ollama(
                id=clean_model,
                host=os.environ.get("OLLAMA_HOST", "http://localhost:11434"),
            )
        else:
            clean_model = model_name
            for prefix in ["openai:", "openai/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix):]
            model = OpenAIChat(id=clean_model)

        # Instantiate the agent
        guidance_block = format_guidance_block(guidance_cfg)

        _call_timer: list[Timer] = []

        def _provider_post_hook(run_output) -> None:
            latency_ms = _call_timer[0].elapsed_ms() if _call_timer else None
            metrics = getattr(run_output, "metrics", None)

            in_t = getattr(metrics, "input_tokens", None)
            out_t = getattr(metrics, "output_tokens", None)
            tot_t = getattr(metrics, "total_tokens", None)
            reas_t = getattr(metrics, "reasoning_tokens", None)

            if in_t:
                token_accumulator["input_tokens"] += in_t
            if out_t:
                token_accumulator["output_tokens"] += out_t
            if tot_t:
                token_accumulator["total_tokens"] += tot_t
            if reas_t:
                token_accumulator["reasoning_tokens"] += reas_t
            token_accumulator["requests"] += 1

            log_kv(
                logger,
                event="provider_call",
                client="agno",
                model_provider=getattr(run_output, "model_provider", None),
                model=getattr(run_output, "model", None),
                latency_ms=latency_ms,
                input_tokens=in_t,
                output_tokens=out_t,
                total_tokens=tot_t,
                reasoning_tokens=reas_t,
                token_scope="call_total",
                tool_calls=(
                    len(getattr(run_output, "tools", []) or [])
                    if getattr(run_output, "tools", None) is not None
                    else None
                ),
            )

        agent = Agent(
            model=model,
            name="DustwoodAgnoMCPAdventurer",
            description=(
                "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
                "You must choose the next game command to execute.\n"
                "Only output a single game command per step (one line, no extra text).\n"
                "Prefer standard parser commands like LOOK, INVENTORY, N/S/E/W, TAKE <item>, USE <item>.\n"
                "Your goal is to survive, explore, and increase your score."
                f"{guidance_block}"
            ),
            markdown=True,
            post_hooks=[_provider_post_hook],
        )

        # Bounded interaction loop
        current_summary = initial_summary
        while (
            last_state is not None
            and last_state.is_playing
            and last_state.turns < max_turns
            and llm_calls < max_llm_calls
        ):
            remaining_turns = max(
                0, max_turns - (last_state.turns if last_state is not None else 0)
            )
            if last_state is not None and not last_state.is_playing:
                logger.info("\n[GAME OVER]")
                break

            recent_history = (
                "\n".join(history[-policy.history_limit :]) if history else "(none)"
            )
            prompt = (
                f"RECENT HISTORY (most recent last):\n{recent_history}\n\n"
                f"CURRENT STATE:\n{current_summary}\n\n"
                f"Remaining game turns: {remaining_turns}\n"
                f"Output exactly one next game command (one line).\n"
                f"Rules: LOOK does not consume a game turn; do not repeat LOOK if turns did not change. "
                f"Exits may not be listed; try NORTH/EAST/SOUTH/WEST to explore when unsure."
            )
            _call_timer.clear()
            _call_timer.append(Timer.start_new())
            run_output = await agent.arun(prompt)
            llm_calls += 1
            raw_cmd = (run_output.content or "").strip()
            if raw_cmd.startswith("```"):
                lines = raw_cmd.splitlines()
                inner = [l for l in lines[1:] if l.strip() != "```"]
                raw_cmd = "\n".join(inner).strip()
            raw_cmd = raw_cmd.splitlines()[0].strip().strip("`").strip()
            raw_cmd = sanitize_command(raw_cmd)
            next_cmd = policy.rewrite(
                proposed_command=raw_cmd,
                state=last_state,
                max_turns=max_turns,
            )
            if not next_cmd:
                raise RuntimeError("Agent produced an empty command.")

            current_summary = await command(next_cmd)
            if last_state is not None:
                policy.observe(
                    command=next_cmd, state=last_state, output_text=last_output_text
                )
                history.append(
                    f"t={last_state.turns} cmd={next_cmd} room={last_state.room_name} score={last_state.score} thirst={last_state.thirst}\n"
                    f"{_trim_output(last_output_text, max_chars=500)}"
                )
                if len(history) > policy.history_limit:
                    history = history[-policy.history_limit :]

        logger.info(f"\n[FINAL STATE]\n{current_summary}")

    except BaseException as e:
        # Catch even SystemExit and KeyboardInterrupt to avoid messy teardown logs if they are just from TaskGroups
        if "TaskGroup" in str(e) or "cancel scope" in str(e):
            logger.debug(f"Suppressed AnyIO/TaskGroup teardown error: {e}")
        else:
            logger.error(f"Error running agent: {e}")
    finally:
        if mcp_tools is not None:
            try:
                await mcp_tools.close()
            except Exception:
                pass
        
        # Log run summary
        log_kv(
            logger,
            event="run_summary",
            client="agno",
            model=model_name,
            latency_ms=run_timer.elapsed_ms(),
            input_tokens=token_accumulator["input_tokens"] or None,
            output_tokens=token_accumulator["output_tokens"] or None,
            total_tokens=token_accumulator["total_tokens"] or None,
            reasoning_tokens=token_accumulator["reasoning_tokens"] or None,
            requests=token_accumulator["requests"] or None,
            token_scope="run_total",
            stop_reason="Agent completed." if last_state is not None and not last_state.is_playing else "Agent stopped or error.",
        )
        # Final grace period outside context manager
        await asyncio.sleep(0.2)


if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS

    asyncio.run(run_agno_mcp_agent(level, model, delay, max_turns))
