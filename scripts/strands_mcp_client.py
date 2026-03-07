import os
import logging
import sys
import time
from typing import Optional
from dotenv import load_dotenv

# Strands Imports
from strands import Agent
from strands.models.litellm import LiteLLMModel
from strands.agent.conversation_manager import SlidingWindowConversationManager
from strands.tools.mcp import MCPClient
from mcp import stdio_client, StdioServerParameters
from strands.hooks import (
    AfterInvocationEvent,
    AfterModelCallEvent,
    AfterToolCallEvent,
    BeforeInvocationEvent,
    BeforeModelCallEvent,
    BeforeToolCallEvent,
)

# Load environment variables
load_dotenv()

# --- Configuration ---
STDIO_PARAMS = StdioServerParameters(
    command="./bin/dustwood-go",
    args=["--turns", "1000"]
)
DEFAULT_MODEL_ID = "gemini/gemini-3-flash-preview"
MESSAGE_HISTORY_LIMIT = 10
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/strands_mcp_client-{EPOCH}.log"

from guidance_loader import load_guidance
from llm_observability import (
    Timer,
    game_console_enabled,
    print_game,
    console_logging_enabled,
    enable_http_debug_logging,
    format_payload,
    http_debug_logging_enabled,
    log_kv,
    provider_payload_logging_enabled,
)

# --- Setup Logging ---
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
os.makedirs("logs", exist_ok=True)

file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s [%(levelname)s] %(message)s'))
logger.addHandler(file_handler)

if console_logging_enabled():
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)
    console_handler.setFormatter(logging.Formatter('%(message)s'))
    logger.addHandler(console_handler)

if http_debug_logging_enabled():
    handlers = [file_handler]
    if console_logging_enabled():
        handlers.append(console_handler)
    enable_http_debug_logging(handlers=handlers)

# Global variable for delay
global_delay = TURN_DELAY

def run_strands_agent(
    level: str,
    model_id: str,
    delay: int,
    max_turns: int,
    transport: str = "streamable-http",
):
    # 1. Initialize the LLM
    llm_model = LiteLLMModel(
        model_id=model_id,
        params={"max_tokens": 4000}
    )

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

    # 3. Setup Conversation Manager
    conv_manager = SlidingWindowConversationManager(window_size=MESSAGE_HISTORY_LIMIT)

    # 4. Initialize Agent with MCP Tools
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }
    guidance_file = guidance_map.get(level, "data/guidance_full.txt")
    guidance_cfg = load_guidance(guidance_file)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")
    
    guidance_block = f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}" if guidance_cfg.text else ""

    agent = Agent(
        model=llm_model,
        system_prompt=(
            "You are an expert text adventure player. Your goal is to play 'Echoes of Dustwood'.\n"
            "Use the 'command' tool to interact with the game. \n"
            "The tool returns both the narrative text and a structured game state.\n"
            "Analyze the state (inventory, thirst, room) to make survival decisions.\n"
            "Always try to survive and increase your score."
            f"{guidance_block}"
        ),
        tools=[mcp_client],
        conversation_manager=conv_manager
    )

    # --- Observability hooks (provider calls + tool calls + latency) ---
    def _before_invocation(event: BeforeInvocationEvent) -> None:
        obs = event.invocation_state.setdefault("_obs", {})
        obs["invocation_start"] = time.perf_counter()
        obs["model_starts"] = []
        log_kv(
            logger,
            event="invocation_start",
            client="strands",
            model=model_id,
            prompt=(format_payload(event.messages) if provider_payload_logging_enabled() else None),
        )

    def _after_invocation(event: AfterInvocationEvent) -> None:
        obs = event.invocation_state.get("_obs", {}) if hasattr(event, "invocation_state") else {}
        start = obs.get("invocation_start")
        invocation_latency_ms = int((time.perf_counter() - start) * 1000) if isinstance(start, (int, float)) else None

        usage = None
        metrics = None
        if event.result is not None and hasattr(event.result, "metrics"):
            usage = getattr(event.result.metrics, "accumulated_usage", None)
            metrics = getattr(event.result.metrics, "accumulated_metrics", None)

        log_kv(
            logger,
            event="provider_call",
            client="strands",
            provider="litellm",
            model=model_id,
            latency_ms=invocation_latency_ms,
            usage=(format_payload(usage) if (usage is not None and provider_payload_logging_enabled()) else None),
            metrics=(format_payload(metrics) if (metrics is not None and provider_payload_logging_enabled()) else None),
            response=(format_payload(str(event.result)) if (event.result is not None and provider_payload_logging_enabled()) else None),
        )

    def _before_model_call(event: BeforeModelCallEvent) -> None:
        obs = event.invocation_state.setdefault("_obs", {})
        obs.setdefault("model_starts", []).append(time.perf_counter())

    def _after_model_call(event: AfterModelCallEvent) -> None:
        obs = event.invocation_state.get("_obs", {})
        starts = obs.get("model_starts") or []
        started = starts.pop() if starts else None
        latency_ms = int((time.perf_counter() - started) * 1000) if isinstance(started, (int, float)) else None
        log_kv(
            logger,
            event="model_call",
            client="strands",
            model=model_id,
            latency_ms=latency_ms,
            stop_reason=(str(event.stop_response.stop_reason) if event.stop_response is not None else None),
        )

    def _before_tool_call(event: BeforeToolCallEvent) -> None:
        obs = event.invocation_state.setdefault("_obs", {})
        tool_starts: dict[str, float] = obs.setdefault("tool_starts", {})
        tool_use_id = (event.tool_use or {}).get("toolUseId") or f"{(event.tool_use or {}).get('name', 'tool')}"
        tool_starts[tool_use_id] = time.perf_counter()
        if game_console_enabled():
            tool_name = (event.tool_use or {}).get("name")
            tool_input = (event.tool_use or {}).get("input") or {}
            if tool_name == "command":
                cmd = tool_input.get("command") if isinstance(tool_input, dict) else None
                if cmd:
                    print_game(f"\n> {cmd}")
        log_kv(
            logger,
            event="tool_call_start",
            client="strands",
            tool_name=(event.tool_use or {}).get("name"),
            tool_use_id=tool_use_id,
            args=(format_payload((event.tool_use or {}).get("input")) if provider_payload_logging_enabled() else None),
        )

    def _after_tool_call(event: AfterToolCallEvent) -> None:
        obs = event.invocation_state.get("_obs", {})
        tool_starts: dict[str, float] = obs.get("tool_starts") or {}
        tool_use_id = (event.tool_use or {}).get("toolUseId") or f"{(event.tool_use or {}).get('name', 'tool')}"
        started = tool_starts.pop(tool_use_id, None)
        latency_ms = int((time.perf_counter() - started) * 1000) if isinstance(started, (int, float)) else None
        if game_console_enabled() and event.exception is None:
            try:
                tool_name = (event.tool_use or {}).get("name")
                if tool_name == "command" and isinstance(event.result, dict):
                    structured = event.result.get("structuredContent")
                    if isinstance(structured, dict):
                        output = structured.get("output") or ""
                        state = structured.get("state") or {}
                        if isinstance(state, dict):
                            turns = state.get("turns")
                            room = state.get("room_name") or state.get("roomName")
                            score = state.get("score")
                            thirst = state.get("thirst")
                            header = f"[turn={turns} room={room} score={score} thirst={thirst}/20]"
                        else:
                            header = "[game]"
                        if output:
                            print_game(f"\n{header}\n{str(output).strip()}\n")
            except Exception:
                pass
        log_kv(
            logger,
            event="tool_call",
            client="strands",
            tool_name=(event.tool_use or {}).get("name"),
            tool_use_id=tool_use_id,
            latency_ms=latency_ms,
            success=(event.exception is None),
            args=(format_payload((event.tool_use or {}).get("input")) if provider_payload_logging_enabled() else None),
            result=(format_payload(event.result) if provider_payload_logging_enabled() else None),
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
    except Exception as e:
        logger.error(f"Error during agent execution: {e}")
    finally:
        try:
            mcp_client.stop(None, None, None)
        except Exception:
            pass

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL_ID
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS
    transport = sys.argv[5] if len(sys.argv) > 5 else "streamable-http"

    run_strands_agent(level, model, delay, max_turns, transport)
