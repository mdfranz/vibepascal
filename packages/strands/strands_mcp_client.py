import contextlib
import logging
import os
import sys
import time
from types import SimpleNamespace
from typing import Any, Optional

import logfire
from dotenv import load_dotenv
from mcp import StdioServerParameters, stdio_client

# Strands Imports
from strands import Agent
from strands.agent.conversation_manager import SlidingWindowConversationManager, SummarizingConversationManager, NullConversationManager
from strands.types.exceptions import EventLoopException
from strands.session.file_session_manager import FileSessionManager
from strands.hooks import (
    AfterInvocationEvent,
    AfterModelCallEvent,
    AfterToolCallEvent,
    BeforeToolCallEvent,
)
from strands.models.litellm import LiteLLMModel
from strands.models.gemini import GeminiModel
from strands.tools.mcp import MCPClient

# Load environment variables
load_dotenv()

# Synchronize API keys for Gemini (LiteLLM uses GEMINI_API_KEY, some others use GOOGLE_API_KEY)
if "GOOGLE_API_KEY" in os.environ and "GEMINI_API_KEY" not in os.environ:
    os.environ["GEMINI_API_KEY"] = os.environ["GOOGLE_API_KEY"]
elif "GEMINI_API_KEY" in os.environ and "GOOGLE_API_KEY" not in os.environ:
    os.environ["GOOGLE_API_KEY"] = os.environ["GEMINI_API_KEY"]

# --- Configuration ---
STDIO_PARAMS = StdioServerParameters(
    command="./bin/dustwood-go", args=["--turns", "1000"]
)
DEFAULT_MODEL_ID = "gemini/gemini-3-flash-preview"
MESSAGE_HISTORY_LIMIT = 10
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/strands_mcp_client-{EPOCH}.log"

class _GameEndedError(EventLoopException):
    """Raised from hooks to break the Strands agent loop when the game ends.

    Subclasses EventLoopException so the event loop bypasses the "cycle failed"
    log path and re-raises cleanly without printing a traceback.
    """

    def __init__(self, message: str) -> None:
        super().__init__(Exception(message))


from vibepascal_shared.guidance_loader import format_guidance_block, load_guidance
from vibepascal_shared.llm_observability import (
    game_console_enabled,
    print_game,
    setup_logger,
)
from vibepascal_shared.mcp_command_policy import CommandPolicy, sanitize_command

logger = setup_logger(__name__, LOG_FILE)

# Global variable for delay
global_delay = TURN_DELAY

# Logfire replaces the old log_kv-based provider_call/tool_call/run_summary telemetry for
# this client, same as pydantic_mcp_client.py. Off by default; opt in with LOGFIRE_ENABLED=1
# (see packages/shared/OBSERVABILITY.md).
LOGFIRE_ENABLED = os.environ.get("LOGFIRE_ENABLED", "0") not in {"0", "false", "False"}
if LOGFIRE_ENABLED:
    # Strands' own OTel instrumentation only records gen_ai.input.messages/gen_ai.output.messages
    # (the actual conversation content) under this opt-in - see
    # https://pydantic.dev/docs/logfire/integrations/llms/strands/. Must be set before the
    # Strands Agent is constructed (in run_strands_agent, below), so set it here at import time.
    os.environ.setdefault(
        "OTEL_SEMCONV_STABILITY_OPT_IN",
        "gen_ai_latest_experimental,gen_ai_span_attributes_only",
    )
    # data_dir defaults to a cwd-relative ".logfire/", but this client is invoked from
    # different working directories (e.g. repo root via strands-mcp-game.sh) - pin it to
    # ~/.logfire/ (same place `logfire auth`/`logfire projects use` write to) so project
    # credentials resolve consistently regardless of cwd.
    logfire.configure(
        service_name="strands-mcp-client",
        environment=os.environ.get("LOGFIRE_ENVIRONMENT", "development"),
        data_dir=os.path.expanduser("~/.logfire"),
    )
    # Strands emits OTel spans for agent invocations, model calls, and tool executions
    # natively (strands.telemetry) - once logfire.configure() sets the global tracer
    # provider, those spans flow to Logfire automatically. There is no
    # logfire.instrument_strands()-style call; configuring before the Agent is created is
    # the whole integration.


def run_strands_agent(
    level: str,
    model_id: str,
    delay: int,
    max_turns: int,
    transport: str = "streamable-http",
    summarize: bool = False,
    windowing: bool = False,
    window_size: int = 6,
    session_id: Optional[str] = None,
):
    # Ensure gemini models use the correct prefix for Gemini API (Google AI Studio)
    # instead of defaulting to Vertex AI if passed without prefix.
    use_native_gemini = False
    if model_id.startswith("google:"):
        model_id = model_id.removeprefix("google:")
        use_native_gemini = True
    elif model_id.startswith("gemini-") and "/" not in model_id:
        use_native_gemini = True
    elif model_id.startswith("gemini/"):
        model_id = model_id.removeprefix("gemini/")
        use_native_gemini = True
    elif model_id.startswith("openai:"):
        model_id = model_id.removeprefix("openai:")
    elif model_id.startswith("anthropic:"):
        model_id = model_id.replace("anthropic:", "anthropic/")

    # 1. Initialize the LLM
    if use_native_gemini:
        api_key = os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY")
        params = {
            "thinking_config": {
                "include_thoughts": True
            }
        }
        if "gemini-2.5" in model_id:
            params["tool_config"] = {
                "function_calling_config": {
                    "mode": "ANY"
                }
            }
        llm_model = GeminiModel(
            model_id=model_id,
            client_args={"api_key": api_key},
            params=params
        )
    else:
        params = {"max_tokens": 20000}
        if "gemini-2.5" in model_id:
            params["tool_choice"] = "required"
        llm_model = LiteLLMModel(model_id=model_id, params=params)

    # 2. Initialize the MCP Client
    if transport == "stdio":
        logger.info("Using Stdio transport for MCP")
        mcp_client = MCPClient(lambda: stdio_client(STDIO_PARAMS))
    else:
        MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
        if transport == "sse":
            logger.info("Using SSE transport for MCP")
            from mcp.client.sse import sse_client

            mcp_client = MCPClient(lambda: sse_client(MCP_URL))
        else:
            logger.info("Using Streamable HTTP transport for MCP")
            from mcp.client.streamable_http import streamablehttp_client

            mcp_client = MCPClient(lambda: streamablehttp_client(MCP_URL))

    # 3. Setup Conversation Manager and Session Manager
    if summarize:
        conv_manager = SummarizingConversationManager(
            summary_ratio=0.3,
            preserve_recent_messages=window_size * 4
        )
    elif windowing:
        conv_manager = SlidingWindowConversationManager(
            window_size=window_size * 4,
            per_turn=True,
        )
    else:
        conv_manager = NullConversationManager()

    active_session_id = session_id if session_id else f"strands-session-{EPOCH}"
    os.makedirs("sessions/strands_sessions", exist_ok=True)
    session_manager = FileSessionManager(
        session_id=active_session_id,
        storage_dir="sessions/strands_sessions"
    )

    # 4. Initialize Agent with MCP Tools
    guidance_cfg = load_guidance(level)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")
    guidance_block = format_guidance_block(guidance_cfg)
    policy = CommandPolicy.from_env()

    last_state_obj: SimpleNamespace | None = None
    last_output_text: str = ""
    _game_over = False

    def _state_obj_from_dict(d: dict) -> SimpleNamespace:
        inv = d.get("inventory") or []
        return SimpleNamespace(
            room_id=int(d.get("room_id") or d.get("roomId") or 0),
            room_name=str(d.get("room_name") or d.get("roomName") or ""),
            turns=int(d.get("turns") or 0),
            score=int(d.get("score") or 0),
            thirst=int(d.get("thirst") or 0),
            is_playing=bool(
                d.get("is_playing") if "is_playing" in d else d.get("isPlaying", True)
            ),
            is_riding=bool(
                d.get("is_riding") if "is_riding" in d else d.get("isRiding", False)
            ),
            has_water=bool(
                d.get("has_water") if "has_water" in d else d.get("hasWater", False)
            ),
            horse_saddled=bool(
                d.get("horse_saddled")
                if "horse_saddled" in d
                else d.get("horseSaddled", False)
            ),
            inventory=list(inv) if isinstance(inv, list) else [],
        )

    agent = Agent(
        model=llm_model,
        system_prompt=(
            "You are an expert text adventure player. Your goal is to play 'Echoes of Dustwood'.\n"
            "Use the 'command' tool to interact with the game. \n"
            "The tool returns both the narrative text and a structured game state.\n"
            "Analyze the state (inventory, thirst, room) to make survival decisions.\n"
            "LOOK does not consume a game turn; do not repeat LOOK if turns did not change.\n"
            "Exits may not be listed; try NORTH/EAST/SOUTH/WEST to explore when unsure.\n"
            "Always try to survive and increase your score."
            f"{guidance_block}"
        ),
        tools=[mcp_client],
        conversation_manager=conv_manager,
        session_manager=session_manager,
        # Tags every span Strands emits for this agent (model calls, tool calls, the
        # top-level invocation) with these attributes - lets Logfire filter/group by run
        # without needing our own wrapper span to carry them. See
        # https://pydantic.dev/docs/logfire/integrations/llms/strands/.
        trace_attributes={
            "session.id": active_session_id,
            "vibepascal.model": model_id,
            "vibepascal.level": level,
            "vibepascal.max_turns": max_turns,
        },
    )

    # --- Hooks: game logic (state tracking, game-over/turn-limit enforcement, command
    # policy) plus a couple of custom Logfire events that Strands' own OTel instrumentation
    # doesn't know about (game_turn, run_summary, game_over). Per-call provider/tool
    # telemetry (latency, token usage, request counts) is no longer hand-tracked here - once
    # LOGFIRE_ENABLED turns on logfire.configure() (above), Strands emits those as spans
    # natively and this client doesn't need to duplicate them.
    def _after_invocation(event: AfterInvocationEvent) -> None:
        if not LOGFIRE_ENABLED:
            return
        # event.result.metrics is only populated when agent() returns normally - but every
        # benchmarked run here ends via _GameEndedError raised from _before_tool_call (turn
        # limit or game over), which leaves event.result None even though the invocation
        # otherwise completed fine (see the `finally:` block around AfterInvocationEvent in
        # strands/agent/agent.py - agent_result is only set from a normal EventLoopStopEvent).
        # agent.event_loop_metrics accumulates usage on the Agent itself as each model call
        # completes, independent of how the invocation ends, so read from there instead.
        usage = getattr(agent.event_loop_metrics, "accumulated_usage", None)

        # accumulated_usage is Strands' own Usage TypedDict - always camelCase
        # (inputTokens/outputTokens/totalTokens) regardless of provider (Gemini, LiteLLM/
        # Anthropic, ...). Snake_case fallbacks are kept only in case a future Strands
        # version changes shape.
        def _get_val(obj, key):
            if obj is None:
                return None
            if isinstance(obj, dict):
                return obj.get(key)
            return getattr(obj, key, None)

        input_tokens = _get_val(usage, "inputTokens") or _get_val(usage, "input_tokens")
        output_tokens = _get_val(usage, "outputTokens") or _get_val(usage, "output_tokens")
        total_tokens = _get_val(usage, "totalTokens") or _get_val(usage, "total_tokens")

        logfire.info(
            "run_summary model={model} input_tokens={input_tokens} output_tokens={output_tokens} "
            "total_tokens={total_tokens}",
            model=model_id,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            total_tokens=total_tokens,
        )

    def _after_model_call(event: AfterModelCallEvent) -> None:
        # THINKING text extraction is a local debugging aid, not telemetry - Strands' own
        # instrumentation already captures the raw model call as a span with token usage.
        if event.stop_response and event.stop_response.message:
            msg = event.stop_response.message
            content_blocks = msg.get("content") or []
            for block in content_blocks:
                rc = block.get("reasoningContent")
                if rc:
                    rt = rc.get("reasoningText")
                    if rt:
                        text = rt.get("text")
                        if text and text.strip():
                            logger.info(f"THINKING: {text.strip()}")

    def _before_tool_call(event: BeforeToolCallEvent) -> None:
        nonlocal _game_over
        # Raise OUTSIDE the inner try/except so it propagates and stops the agent loop.
        if _game_over:
            raise _GameEndedError("Game ended — stopping agent loop")
        try:
            tool_name = (event.tool_use or {}).get("name")
            tool_input = (event.tool_use or {}).get("input") or {}
            # Apply game-state checks to ALL MCP tools (go, take, drop, drink, command…),
            # not just "command" — the agent uses named tools that also consume game turns
            # and return structuredContent, so last_state_obj reflects the real turn count.
            if last_state_obj is not None and not last_state_obj.is_playing:
                _game_over = True
                event.cancel_tool = "Game is over."
                return
            if last_state_obj is not None and int(
                getattr(last_state_obj, "turns", 0)
            ) >= int(max_turns):
                _game_over = True
                event.cancel_tool = "Turn limit reached."
                return
            if tool_name == "command" and isinstance(tool_input, dict):
                raw_cmd = sanitize_command(str(tool_input.get("command") or ""))
                if last_state_obj is not None:
                    rewritten = policy.rewrite(
                        proposed_command=raw_cmd,
                        state=last_state_obj,
                        max_turns=max_turns,
                    )
                else:
                    rewritten = raw_cmd or "LOOK"
                tool_input["command"] = rewritten
                event.tool_use["input"] = tool_input
        except Exception:
            pass
        if game_console_enabled():
            tool_name = (event.tool_use or {}).get("name")
            tool_input = (event.tool_use or {}).get("input") or {}
            if tool_name == "command":
                cmd = (
                    tool_input.get("command") if isinstance(tool_input, dict) else None
                )
                if cmd:
                    print_game(f"\n> {cmd}")

    def _after_tool_call(event: AfterToolCallEvent) -> None:
        nonlocal last_state_obj
        nonlocal last_output_text
        nonlocal _game_over
        if event.exception is None:
            try:
                tool_name = (event.tool_use or {}).get("name")
                # Update game state from ALL tools that return structuredContent
                # (go, take, drop, drink, command…), not just "command".
                if isinstance(event.result, dict):
                    structured = event.result.get("structuredContent")
                    if isinstance(structured, dict):
                        output = structured.get("output") or ""
                        state = structured.get("state") or {}
                        if isinstance(state, dict):
                            last_state_obj = _state_obj_from_dict(state)
                            last_output_text = str(output or "")
                            if tool_name == "command":
                                tool_input = (event.tool_use or {}).get("input") or {}
                                executed_cmd = (
                                    sanitize_command(str(tool_input.get("command") or ""))
                                    if isinstance(tool_input, dict)
                                    else ""
                                )
                                if executed_cmd and last_state_obj is not None:
                                    policy.observe(
                                        command=executed_cmd,
                                        state=last_state_obj,
                                        output_text=last_output_text,
                                    )
                            if LOGFIRE_ENABLED:
                                # Custom game-domain event Strands' own instrumentation
                                # doesn't know about, mirroring pydantic_mcp_client.py's
                                # game_turn event - lets Logfire chart score-over-turns
                                # directly from trace data.
                                logfire.info(
                                    "game_turn {turn} room={room} score={score} thirst={thirst}",
                                    turn=state.get("turns", 0),
                                    room=state.get("room_name") or state.get("roomName") or "Unknown",
                                    score=state.get("score", 0),
                                    thirst=state.get("thirst", 0),
                                )
                        if game_console_enabled():
                            if isinstance(state, dict):
                                turns = state.get("turns")
                                room = state.get("room_name") or state.get("roomName")
                                score = state.get("score")
                                thirst = state.get("thirst")
                                header = f"[turn={turns} room={room} score={score} thirst={thirst}]"
                            else:
                                header = "[game]"
                            if output:
                                print_game(f"\n{header}\n{str(output).strip()}\n")
            except Exception:
                pass
        # Detect server-reported turn limit (error response with no structuredContent).
        # Without this, last_state_obj.turns never reaches max_turns and the LLM
        # spins calling QUIT repeatedly after the server rejects every command.
        if (
            event.exception is None
            and not getattr(event, "cancel_message", None)
            and isinstance(event.result, dict)
            and event.result.get("status") == "error"
        ):
            for c in event.result.get("content") or []:
                if isinstance(c, dict) and "Turn limit reached" in c.get("text", ""):
                    _game_over = True
                    logger.info("Game over: server turn limit reached")
                    if LOGFIRE_ENABLED:
                        logfire.info("game_over reason={reason}", reason="server_turn_limit")
                    break

    agent.add_hook(_after_invocation, AfterInvocationEvent)
    agent.add_hook(_after_model_call, AfterModelCallEvent)
    agent.add_hook(_before_tool_call, BeforeToolCallEvent)
    agent.add_hook(_after_tool_call, AfterToolCallEvent)

    logger.info(f"--- Strands MCP Agent Starting (Model: {model_id}) ---")

    prompt = (
        "Start by calling the 'command' tool with command='LOOK' and reset=True. "
        f"Then continue playing for up to {max_turns} turns to increase your score."
    )

    # Mirrors pydantic_mcp_client.py's pydantic_game_run span: wraps the whole session so
    # every span Strands emits (model calls, tool calls) plus the custom game_turn/
    # run_summary/game_over events above share one trace_id.
    run_span = (
        logfire.span(
            "strands_game_run",
            model=model_id,
            level=level,
            max_turns=max_turns,
            session_id=active_session_id,
        )
        if LOGFIRE_ENABLED
        else contextlib.nullcontext()
    )
    # Some providers/models intermittently return a fully empty completion - no text, no
    # tool call, finish_reason="stop" - which Strands treats as a normal final answer and
    # ends the agent loop. Reproduced live and in isolation against
    # openrouter/google/gemini-3.6-flash: ~50-60% empty-response rate replaying the exact
    # same turn repeatedly, unaffected by tool_choice or excluding reasoning output - this
    # is provider-side flakiness, not something a request parameter fixes deterministically.
    # Left unhandled, one unlucky completion silently truncates an otherwise-healthy run to
    # a handful of turns. Mitigate the only way that matches the actual failure mode: when
    # the agent stops with empty output while the game is still demonstrably active (per
    # last_state_obj from the tool-call hooks above), nudge it to continue, bounded so a
    # persistently broken model still terminates.
    #
    # Strands' own streaming.py:_normalize_messages replaces a truly empty assistant
    # content list with the literal sentinel text "[blank text]" before it's stored on the
    # message/AgentResult - str(result) is therefore "[blank text]", not "", on this path.
    # Treat both as empty.
    MAX_EMPTY_STOP_RETRIES = 5

    def _is_empty_result(r: Any) -> bool:
        text = str(r).strip()
        return text in ("", "[blank text]")

    try:
        with run_span:
            result = agent(prompt)
            empty_stop_retries = 0
            while (
                _is_empty_result(result)
                and last_state_obj is not None
                and last_state_obj.is_playing
                and last_state_obj.turns < max_turns
                and empty_stop_retries < MAX_EMPTY_STOP_RETRIES
            ):
                empty_stop_retries += 1
                logger.info(
                    f"Model stopped with an empty response at turn {last_state_obj.turns}/{max_turns} "
                    f"while the game is still active - retrying ({empty_stop_retries}/{MAX_EMPTY_STOP_RETRIES})."
                )
                if LOGFIRE_ENABLED:
                    logfire.info(
                        "empty_stop_retry attempt={attempt} turn={turn}",
                        attempt=empty_stop_retries,
                        turn=last_state_obj.turns,
                    )
                result = agent("Continue playing by calling an MCP tool. The game is not over.")
            logger.info(f"\n[FINAL AGENT RESPONSE]\n{str(result).strip()}")
    except _GameEndedError as e:
        logger.info(f"Game ended: {e}")
    except Exception as e:
        # Defense in depth, matching pydantic_mcp_client.py: any other error (MCP transport
        # issues, provider outages, etc.) should end this run's game loop gracefully rather
        # than crashing a serial multi-model benchmark run outright.
        logger.error(f"Error during agent execution: {e}")
        if LOGFIRE_ENABLED:
            logfire.exception("game_run_failed model={model}", model=model_id)
    finally:
        try:
            mcp_client.stop(None, None, None)
        except Exception:
            pass


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("level", nargs="?", default="full")
    parser.add_argument("model", nargs="?", default=DEFAULT_MODEL_ID)
    parser.add_argument("delay", nargs="?", type=int, default=TURN_DELAY)
    parser.add_argument("max_turns", nargs="?", type=int, default=MAX_TURNS)
    parser.add_argument("transport", nargs="?", default="streamable-http")
    parser.add_argument("--summarize", "-s", action="store_true", help="Enable summarization")
    parser.add_argument("--windowing", "-w", action="store_true", help="Enable sliding window history (disabled by default)")
    parser.add_argument("--window-size", "-n", type=int, default=6, help="Window size in game turns (default: 6)")
    parser.add_argument("--session-id", type=str, default=None, help="Session ID to restore or create")

    args = parser.parse_args()

    run_strands_agent(
        level=args.level,
        model_id=args.model,
        delay=args.delay,
        max_turns=args.max_turns,
        transport=args.transport,
        summarize=args.summarize,
        windowing=args.windowing,
        window_size=args.window_size,
        session_id=args.session_id,
    )
