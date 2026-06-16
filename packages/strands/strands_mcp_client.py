import logging
import os
import sys
import time
from types import SimpleNamespace
from typing import Optional

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
    BeforeInvocationEvent,
    BeforeModelCallEvent,
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
    Timer,
    format_payload,
    game_console_enabled,
    log_kv,
    print_game,
    provider_payload_logging_enabled,
    setup_logger,
)
from vibepascal_shared.mcp_command_policy import CommandPolicy, sanitize_command

logger = setup_logger(__name__, LOG_FILE)

# Global variable for delay
global_delay = TURN_DELAY


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
    )

    # --- Observability hooks (provider calls + tool calls + latency) ---
    def _before_invocation(event: BeforeInvocationEvent) -> None:
        obs = event.invocation_state.setdefault("_obs", {})
        obs["invocation_start"] = time.perf_counter()
        obs["model_starts"] = []
        obs["requests"] = 0
        log_kv(
            logger,
            event="invocation_start",
            client="strands",
            model=model_id,
            prompt=(
                format_payload(event.messages)
                if provider_payload_logging_enabled()
                else None
            ),
        )

    def _after_invocation(event: AfterInvocationEvent) -> None:
        obs = (
            event.invocation_state.get("_obs", {})
            if hasattr(event, "invocation_state")
            else {}
        )
        start = obs.get("invocation_start")
        invocation_latency_ms = (
            int((time.perf_counter() - start) * 1000)
            if isinstance(start, (int, float))
            else None
        )

        usage = None
        result_metrics = None
        if event.result is not None and hasattr(event.result, "metrics"):
            usage = getattr(event.result.metrics, "accumulated_usage", None)
            result_metrics = getattr(event.result.metrics, "accumulated_metrics", None)

        # Normalize token fields from LiteLLM accumulated_usage
        input_tokens = None
        output_tokens = None
        total_tokens = None
        if usage is not None:
            def _get_val(obj, key):
                if isinstance(obj, dict):
                    return obj.get(key)
                return getattr(obj, key, None)

            input_tokens = (
                _get_val(usage, "prompt_tokens")
                or _get_val(usage, "input_tokens")
                or _get_val(usage, "inputTokens")
            )
            output_tokens = (
                _get_val(usage, "completion_tokens")
                or _get_val(usage, "output_tokens")
                or _get_val(usage, "outputTokens")
            )
            total_tokens = (
                _get_val(usage, "total_tokens")
                or _get_val(usage, "totalTokens")
            )

        log_kv(
            logger,
            event="provider_call",
            client="strands",
            provider="litellm",
            model=model_id,
            latency_ms=invocation_latency_ms,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            total_tokens=total_tokens,
            token_scope="run_total",
            usage=(
                format_payload(usage)
                if (usage is not None and provider_payload_logging_enabled())
                else None
            ),
            metrics=(
                format_payload(result_metrics)
                if (result_metrics is not None and provider_payload_logging_enabled())
                else None
            ),
            response=(
                format_payload(str(event.result))
                if (event.result is not None and provider_payload_logging_enabled())
                else None
            ),
        )

        requests = obs.get("requests", 0)
        log_kv(
            logger,
            event="run_summary",
            client="strands",
            model=model_id,
            latency_ms=invocation_latency_ms,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            total_tokens=total_tokens,
            requests=requests,
            token_scope="run_total",
            stop_reason="Agent completed.",
        )

    def _before_model_call(event: BeforeModelCallEvent) -> None:
        obs = event.invocation_state.setdefault("_obs", {})
        obs.setdefault("model_starts", []).append(time.perf_counter())

    def _after_model_call(event: AfterModelCallEvent) -> None:
        obs = event.invocation_state.get("_obs", {})
        obs["requests"] = obs.get("requests", 0) + 1
        starts = obs.get("model_starts") or []
        started = starts.pop() if starts else None
        latency_ms = (
            int((time.perf_counter() - started) * 1000)
            if isinstance(started, (int, float))
            else None
        )
        log_kv(
            logger,
            event="model_call",
            client="strands",
            model=model_id,
            latency_ms=latency_ms,
            stop_reason=(
                str(event.stop_response.stop_reason)
                if event.stop_response is not None
                else None
            ),
        )
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
        obs = event.invocation_state.setdefault("_obs", {})
        tool_starts: dict[str, float] = obs.setdefault("tool_starts", {})
        tool_use_id = (event.tool_use or {}).get(
            "toolUseId"
        ) or f"{(event.tool_use or {}).get('name', 'tool')}"
        tool_starts[tool_use_id] = time.perf_counter()
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
        log_kv(
            logger,
            event="tool_call_start",
            client="strands",
            tool_name=(event.tool_use or {}).get("name"),
            tool_use_id=tool_use_id,
            args=(
                format_payload((event.tool_use or {}).get("input"))
                if provider_payload_logging_enabled()
                else None
            ),
        )

    def _after_tool_call(event: AfterToolCallEvent) -> None:
        nonlocal last_state_obj
        nonlocal last_output_text
        nonlocal _game_over
        obs = event.invocation_state.get("_obs", {})
        tool_starts: dict[str, float] = obs.get("tool_starts") or {}
        tool_use_id = (event.tool_use or {}).get(
            "toolUseId"
        ) or f"{(event.tool_use or {}).get('name', 'tool')}"
        started = tool_starts.pop(tool_use_id, None)
        latency_ms = (
            int((time.perf_counter() - started) * 1000)
            if isinstance(started, (int, float))
            else None
        )
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
                    log_kv(logger, event="game_over", client="strands", reason="server_turn_limit")
                    break
        log_kv(
            logger,
            event="tool_call",
            client="strands",
            tool_name=(event.tool_use or {}).get("name"),
            tool_use_id=tool_use_id,
            latency_ms=latency_ms,
            success=(event.exception is None),
            args=(
                format_payload((event.tool_use or {}).get("input"))
                if provider_payload_logging_enabled()
                else None
            ),
            result=(
                format_payload(event.result)
                if provider_payload_logging_enabled()
                else None
            ),
            error=(str(event.exception) if event.exception is not None else None),
        )

    agent.add_hook(_before_invocation, BeforeInvocationEvent)
    agent.add_hook(_after_invocation, AfterInvocationEvent)
    agent.add_hook(_before_model_call, BeforeModelCallEvent)
    agent.add_hook(_after_model_call, AfterModelCallEvent)
    agent.add_hook(_before_tool_call, BeforeToolCallEvent)
    agent.add_hook(_after_tool_call, AfterToolCallEvent)

    logger.info(f"--- Strands MCP Agent Starting (Model: {model_id}) ---")

    prompt = (
        "Start by calling the 'command' tool with command='LOOK' and reset=True. "
        f"Then continue playing for up to {max_turns} turns to increase your score."
    )

    try:
        result = agent(prompt)
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{str(result).strip()}")
    except _GameEndedError as e:
        logger.info(f"Game ended: {e}")
    except Exception as e:
        logger.error(f"Error during agent execution: {e}")
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
