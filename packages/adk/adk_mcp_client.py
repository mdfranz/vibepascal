import asyncio
import logging
import os
import sys
import time
from dataclasses import dataclass
from typing import Any, Optional

from dotenv import load_dotenv
from google.adk import Agent, Runner
from google.adk.agents.run_config import RunConfig
from google.adk.agents.invocation_context import LlmCallsLimitExceededError
from google.adk.models.lite_llm import LiteLlm
from google.adk.sessions import InMemorySessionService
from google.adk.tools.mcp_tool import McpToolset
from google.adk.tools.mcp_tool.mcp_session_manager import StreamableHTTPConnectionParams
from google.genai import types
from vibepascal_shared.guidance_loader import load_guidance
from vibepascal_shared.llm_observability import (
    Timer,
    console_logging_enabled,
    enable_http_debug_logging,
    format_payload,
    game_console_enabled,
    http_debug_logging_enabled,
    log_kv,
    print_game,
    provider_payload_logging_enabled,
)

# Load environment variables
load_dotenv()

# Keep Gemini key aliases compatible with other framework scripts.
if "GOOGLE_API_KEY" in os.environ and "GEMINI_API_KEY" not in os.environ:
    os.environ["GEMINI_API_KEY"] = os.environ["GOOGLE_API_KEY"]
elif "GEMINI_API_KEY" in os.environ and "GOOGLE_API_KEY" not in os.environ:
    os.environ["GOOGLE_API_KEY"] = os.environ["GEMINI_API_KEY"]

# --- Configuration ---
MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
DEFAULT_MODEL = "gemini-3.5-flash"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/adk_mcp_client-{EPOCH}.log"


@dataclass
class GameState:
    room_name: str
    turns: int
    score: int
    thirst: int
    is_playing: bool

    @classmethod
    def from_dict(cls, value: dict[str, Any]) -> "GameState":
        return cls(
            room_name=str(value.get("room_name") or value.get("roomName") or "Unknown"),
            turns=int(value.get("turns") or 0),
            score=int(value.get("score") or 0),
            thirst=int(value.get("thirst") or 0),
            is_playing=bool(
                value.get("is_playing")
                if "is_playing" in value
                else value.get("isPlaying", True)
            ),
        )


# --- Setup Logging ---
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
os.makedirs("logs", exist_ok=True)

file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter("%(asctime)s [%(levelname)s] %(message)s"))
logger.addHandler(file_handler)

if console_logging_enabled():
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)
    console_handler.setFormatter(logging.Formatter("%(message)s"))
    logger.addHandler(console_handler)

if http_debug_logging_enabled():
    handlers = [file_handler]
    if console_logging_enabled():
        handlers.append(console_handler)
    enable_http_debug_logging(handlers=handlers)
else:
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("httpcore").setLevel(logging.WARNING)


def _to_plain(value: Any) -> Any:
    if value is None:
        return None
    if isinstance(value, (str, int, float, bool, dict, list)):
        return value
    if hasattr(value, "model_dump"):
        try:
            return value.model_dump(mode="json", exclude_none=True)
        except Exception:
            pass
    if hasattr(value, "dict"):
        try:
            return value.dict()
        except Exception:
            pass
    return value


def _normalize_model_name(model_name: str) -> str:
    model = (model_name or "").strip()
    if not model:
        return DEFAULT_MODEL

    if model.startswith("google-gla:"):
        return model.removeprefix("google-gla:")
    if model.startswith("google:"):
        return model.removeprefix("google:")
    if model.startswith("gemini/"):
        return model.removeprefix("gemini/")
    return model


def _resolve_model(model_name: str) -> tuple[Any, str]:
    """Resolve a user model string to ADK native or LiteLLM model object."""
    model = _normalize_model_name(model_name)
    lower = model.lower()

    # Native Gemini path in ADK.
    if lower.startswith("gemini-"):
        return model, model

    # Provider-prefixed model strings (LiteLLM format).
    if "/" in model:
        provider, rest = model.split("/", 1)
        if provider.lower() == "gemini":
            return rest, rest
        return LiteLlm(model=model), model

    # Common prefixed shorthands.
    if model.startswith("openai:"):
        litellm_model = f"openai/{model.removeprefix('openai:')}"
        return LiteLlm(model=litellm_model), litellm_model
    if model.startswith("anthropic:"):
        litellm_model = f"anthropic/{model.removeprefix('anthropic:')}"
        return LiteLlm(model=litellm_model), litellm_model
    if model.startswith("ollama:"):
        litellm_model = f"ollama/{model.removeprefix('ollama:')}"
        return LiteLlm(model=litellm_model), litellm_model

    # Bare shorthand model names.
    if lower.startswith(("gpt-", "o1", "o3", "o4", "chatgpt-")):
        litellm_model = f"openai/{model}"
        return LiteLlm(model=litellm_model), litellm_model
    if lower.startswith("claude-"):
        litellm_model = f"anthropic/{model}"
        return LiteLlm(model=litellm_model), litellm_model

    # Fallback: treat as LiteLLM model string.
    return LiteLlm(model=model), model


def _extract_text(content: Any) -> str:
    if content is None:
        return ""
    parts = getattr(content, "parts", None) or []
    texts: list[str] = []
    for part in parts:
        text = getattr(part, "text", None)
        if isinstance(text, str) and text.strip():
            texts.append(text.strip())
    return "\n".join(texts).strip()


def _extract_state_and_output(response_payload: Any) -> tuple[Optional[GameState], str]:
    payload = _to_plain(response_payload)
    if not isinstance(payload, dict):
        return None, ""

    structured = payload.get("structuredContent")
    if not isinstance(structured, dict):
        structured = payload if "state" in payload else None
    if not isinstance(structured, dict):
        return None, ""

    state_raw = structured.get("state")
    output = structured.get("output")
    if not isinstance(output, str):
        output = ""

    state: Optional[GameState] = None
    if isinstance(state_raw, dict):
        state = GameState.from_dict(state_raw)
    return state, output


def _make_after_model_callback(resolved_model_id: str, token_accumulator: dict):
    def _after_model_callback(callback_context, llm_response):
        usage = getattr(llm_response, "usage_metadata", None)
        if usage is None:
            return None
        prompt_tokens = getattr(usage, "prompt_token_count", None)
        candidates_tokens = getattr(usage, "candidates_token_count", None)
        total_tokens = getattr(usage, "total_token_count", None)
        thoughts_tokens = getattr(usage, "thoughts_token_count", None) or None
        cached_tokens = getattr(usage, "cached_content_token_count", None) or None

        if prompt_tokens:
            token_accumulator["input_tokens"] += prompt_tokens
        if candidates_tokens:
            token_accumulator["output_tokens"] += candidates_tokens
        if total_tokens:
            token_accumulator["total_tokens"] += total_tokens
        if cached_tokens:
            token_accumulator["cache_read_tokens"] += cached_tokens
        token_accumulator["requests"] += 1

        log_kv(
            logger,
            event="provider_call",
            client="adk",
            model=resolved_model_id,
            input_tokens=prompt_tokens,
            output_tokens=candidates_tokens,
            total_tokens=total_tokens,
            reasoning_tokens=thoughts_tokens,
            cache_read_tokens=cached_tokens,
            token_scope="call_total",
        )
        return None
    return _after_model_callback


async def run_adk_mcp_agent(level: str, model_name: str, delay: int, max_turns: int):
    model, resolved_model_id = _resolve_model(model_name)
    logger.info(f"--- ADK MCP Client Starting (Model: {resolved_model_id}) ---")

    token_accumulator = {
        "input_tokens": 0,
        "output_tokens": 0,
        "total_tokens": 0,
        "cache_read_tokens": 0,
        "requests": 0,
    }

    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt",
    }
    guidance_file = guidance_map.get(level, "data/guidance_full.txt")
    guidance_cfg = load_guidance(guidance_file)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")

    guidance_block = (
        f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}" if guidance_cfg.text else ""
    )

    connection_params = StreamableHTTPConnectionParams(
        url=MCP_URL,
        timeout=10.0,
        sse_read_timeout=300.0,
        terminate_on_close=True,
    )
    toolset = McpToolset(connection_params=connection_params, tool_filter=["command"])
    agent = Agent(
        name="dustwood_adk_agent",
        model=model,
        after_model_callback=_make_after_model_callback(resolved_model_id, token_accumulator),
        instruction=(
            "You are an expert adventurer playing 'Echoes of Dustwood' via MCP.\n"
            "Use the `command` tool for every game interaction.\n"
            "Start with command='LOOK' and reset=True.\n"
            "LOOK does not consume a game turn; avoid repeating LOOK if turns are unchanged.\n"
            "Prefer parser-friendly commands: LOOK, INVENTORY, N/S/E/W, TAKE <item>, FIX <item>.\n"
            f"Stop after {max_turns} turns or when the game ends."
            f"{guidance_block}"
        ),
        tools=[toolset],
    )

    app_name = "dustwood_adk_mcp"
    user_id = "adk_user"
    session_id = f"adk-session-{EPOCH}"
    session_service = InMemorySessionService()
    await session_service.create_session(
        app_name=app_name, user_id=user_id, session_id=session_id
    )
    runner = Runner(agent=agent, app_name=app_name, session_service=session_service)

    prompt = (
        "Play Echoes of Dustwood now.\n"
        "First call the command tool with command='LOOK' and reset=True.\n"
        f"Then keep playing to maximize score for up to {max_turns} turns.\n"
        "If turns reach the limit, provide a short final summary."
    )
    content = types.Content(role="user", parts=[types.Part(text=prompt)])

    processed_event_ids: set[str] = set()
    final_text = ""
    stop_reason = ""
    limit_reached = False
    run_timer = Timer.start_new()

    try:
        try:
            async for event in runner.run_async(
                user_id=user_id,
                session_id=session_id,
                new_message=content,
                run_config=RunConfig(max_llm_calls=max(8, max_turns * 4)),
            ):
                event_id = getattr(event, "id", None)
                if event_id in processed_event_ids:
                    continue
                if event_id:
                    processed_event_ids.add(event_id)

                if getattr(event, "partial", False):
                    continue

                event_text = _extract_text(getattr(event, "content", None))
                if event_text:
                    logger.info(f"AI: {event_text}")
                    final_text = event_text

                for fc in event.get_function_calls():
                    args = _to_plain(getattr(fc, "args", None))
                    logger.info(f"TOOL: {fc.name}({args})")
                    log_kv(
                        logger,
                        event="tool_intent",
                        client="adk",
                        tool_name=fc.name,
                        args=(
                            format_payload(args)
                            if provider_payload_logging_enabled()
                            else None
                        ),
                    )

                for fr in event.get_function_responses():
                    payload = _to_plain(getattr(fr, "response", None))
                    log_kv(
                        logger,
                        event="tool_call",
                        client="adk",
                        tool_name=getattr(fr, "name", "unknown"),
                        result=(
                            format_payload(payload)
                            if provider_payload_logging_enabled()
                            else None
                        ),
                    )

                    state, output = _extract_state_and_output(payload)
                    if state and output:
                        if game_console_enabled():
                            print_game(
                                f"\n[turn={state.turns} room={state.room_name} "
                                f"score={state.score} thirst={state.thirst}]\n"
                                f"{output.strip()}\n"
                            )

                        if delay > 0:
                            await asyncio.sleep(delay)

                        if not state.is_playing:
                            stop_reason = "Game ended."
                        elif state.turns >= max_turns and not limit_reached:
                            limit_reached = True
                            stop_reason = f"Turn limit ({max_turns}) reached."
                            logger.info(f"{stop_reason} Waiting for ADK run to conclude cleanly.")

                if getattr(event, "is_final_response", lambda: False)():
                    # Keep iterating until stop condition, but preserve latest final text.
                    pass
        except LlmCallsLimitExceededError as e:
            if not stop_reason:
                stop_reason = f"ADK max llm calls reached: {e}"
            logger.info(stop_reason)
        except ValueError as e:
            msg = str(e)
            if "Model " in msg and "not found" in msg:
                stop_reason = f"Unsupported ADK model: {resolved_model_id}"
                logger.error(f"{stop_reason}. Use Gemini IDs or LiteLLM provider strings like openai/gpt-5-mini.")
            else:
                raise
    finally:
        await runner.close()
        await toolset.close()

    log_kv(
        logger,
        event="run_summary",
        client="adk",
        model=resolved_model_id,
        latency_ms=run_timer.elapsed_ms(),
        input_tokens=token_accumulator["input_tokens"] or None,
        output_tokens=token_accumulator["output_tokens"] or None,
        total_tokens=token_accumulator["total_tokens"] or None,
        cache_read_tokens=token_accumulator["cache_read_tokens"] or None,
        requests=token_accumulator["requests"] or None,
        token_scope="run_total",
        stop_reason=stop_reason or "Agent completed.",
    )
    if final_text:
        logger.info(f"\n[FINAL ADK RESPONSE]\n{final_text}")


if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS

    asyncio.run(run_adk_mcp_agent(level, model, delay, max_turns))
