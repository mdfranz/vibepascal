import os
import asyncio
import logging
import sys
import time
import json
from typing import Optional, Any, List
from pydantic import BaseModel
from dotenv import load_dotenv

from agent_framework import Agent
from agent_framework.openai import OpenAIChatClient
from agent_framework.anthropic import AnthropicClient
from agent_framework.ollama import OllamaChatClient
from agent_framework._mcp import MCPStreamableHTTPTool # Using the official MCP tool integration

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

# Load environment variables
load_dotenv()

# --- Configuration ---
MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
DEFAULT_MODEL = "gpt-5-mini"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/ms_agent_mcp_client-{EPOCH}.log"

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

# Silence verbose loggers
if http_debug_logging_enabled():
    handlers = [file_handler]
    if console_logging_enabled():
        handlers.append(console_handler)
    enable_http_debug_logging(handlers=handlers)
else:
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("httpcore").setLevel(logging.WARNING)

# --- State Models for Parsing ---

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

def mcp_result_parser(result: Any) -> str:
    """Parses and logs the MCP tool result."""
    if hasattr(result, "structuredContent") and result.structuredContent:
        try:
            parsed = CommandOutput(**result.structuredContent)
            state = parsed.state
            
            summary_str = (
                f"--- Game State ---\n"
                f"Room: {state.room_name} (ID: {state.room_id})\n"
                f"Turns: {state.turns}, Score: {state.score}, Thirst: {state.thirst}/20\n"
                f"Inventory: {', '.join(state.inventory) if state.inventory else 'Empty'}\n"
                f"Status: Riding={state.is_riding}, Saddled={state.horse_saddled}, Water={state.has_water}\n"
                f"-----------------\n\n"
                f"{parsed.output}"
            )
            logger.info(f"Game output received (State: {state.room_name})")
            if game_console_enabled():
                print_game(
                    f"\n[turn={state.turns} room={state.room_name} score={state.score} thirst={state.thirst}/20]\n"
                    f"{parsed.output.strip()}\n"
                )
            return summary_str
        except Exception as e:
            logger.warning(f"Failed to parse structuredContent: {e}")
    
    if hasattr(result, "content"):
        text = "\n".join([c.text for c in result.content if hasattr(c, "text")])
        return text
    
    return str(result)

def _serialize_messages(messages: Any) -> Any:
    try:
        return [getattr(m, "to_dict")() if hasattr(m, "to_dict") else str(m) for m in (messages or [])]
    except Exception:
        return str(messages)


def _extract_tool_calls_from_response(response: Any) -> list[dict[str, Any]]:
    tool_calls: list[dict[str, Any]] = []
    try:
        for msg in getattr(response, "messages", []) or []:
            for content in getattr(msg, "contents", []) or []:
                ctype = getattr(content, "type", None)
                if ctype in {
                    "function_call",
                    "mcp_server_tool_call",
                    "shell_tool_call",
                    "code_interpreter_tool_call",
                    "image_generation_tool_call",
                }:
                    tool_calls.append(
                        {
                            "type": ctype,
                            "call_id": getattr(content, "call_id", None),
                            "tool_name": getattr(content, "tool_name", None),
                            "server_name": getattr(content, "server_name", None),
                            "arguments": getattr(content, "arguments", None),
                        }
                    )
    except Exception:
        return []
    return tool_calls


class LoggingChatClient:
    """Wrap a chat client to log provider calls, usage, tool calls, and latency."""

    def __init__(self, inner: Any, *, client_name: str, default_model_id: str):
        self._inner = inner
        self._client_name = client_name
        self._default_model_id = default_model_id

    @property
    def additional_properties(self) -> dict[str, Any]:
        return getattr(self._inner, "additional_properties", {})

    def get_response(self, messages: Any, *, stream: bool = False, options: Any = None, **kwargs: Any):
        request_timer = Timer.start_new()
        request_payload = _serialize_messages(messages) if provider_payload_logging_enabled() else None

        try:
            inner_result = self._inner.get_response(messages, stream=stream, options=options, **kwargs)
        except Exception as e:
            log_kv(
                logger,
                level="error",
                event="provider_call",
                client="ms_agent",
                provider=self._client_name,
                model=self._default_model_id,
                latency_ms=request_timer.elapsed_ms(),
                request=(format_payload(request_payload) if request_payload is not None else None),
                error=str(e),
            )
            raise

        if stream and hasattr(inner_result, "with_result_hook"):
            def _hook(final_response: Any):
                usage_details = getattr(final_response, "usage_details", None)
                tool_calls = _extract_tool_calls_from_response(final_response)
                log_kv(
                    logger,
                    event="provider_call",
                    client="ms_agent",
                    provider=self._client_name,
                    model=getattr(final_response, "model_id", None) or self._default_model_id,
                    latency_ms=request_timer.elapsed_ms(),
                    usage=(format_payload(usage_details) if (usage_details is not None and provider_payload_logging_enabled()) else None),
                    tool_call_count=len(tool_calls) if tool_calls else 0,
                    tool_calls=(format_payload(tool_calls) if (tool_calls and provider_payload_logging_enabled()) else None),
                    request=(format_payload(request_payload) if request_payload is not None else None),
                    response=(format_payload(getattr(final_response, "text", None)) if provider_payload_logging_enabled() else None),
                )
                return final_response

            return inner_result.with_result_hook(_hook)

        async def _await_and_log():
            try:
                final_response = await inner_result
            except Exception as e:
                log_kv(
                    logger,
                    level="error",
                    event="provider_call",
                    client="ms_agent",
                    provider=self._client_name,
                    model=self._default_model_id,
                    latency_ms=request_timer.elapsed_ms(),
                    request=(format_payload(request_payload) if request_payload is not None else None),
                    error=str(e),
                )
                raise

            usage_details = getattr(final_response, "usage_details", None)
            tool_calls = _extract_tool_calls_from_response(final_response)
            log_kv(
                logger,
                event="provider_call",
                client="ms_agent",
                provider=self._client_name,
                model=getattr(final_response, "model_id", None) or self._default_model_id,
                latency_ms=request_timer.elapsed_ms(),
                usage=(format_payload(usage_details) if (usage_details is not None and provider_payload_logging_enabled()) else None),
                tool_call_count=len(tool_calls) if tool_calls else 0,
                tool_calls=(format_payload(tool_calls) if (tool_calls and provider_payload_logging_enabled()) else None),
                request=(format_payload(request_payload) if request_payload is not None else None),
                response=(format_payload(getattr(final_response, "text", None)) if provider_payload_logging_enabled() else None),
            )
            return final_response

        return _await_and_log()


class DelayedMCPStreamableHTTPTool(MCPStreamableHTTPTool):
    """Subclass to add delay and logging between tool calls."""
    def __init__(self, *args, delay: int = 0, **kwargs):
        super().__init__(*args, **kwargs)
        self.call_delay = delay

    async def call_tool(self, tool_name: str, **kwargs: Any) -> str:
        cmd_val = kwargs.get("command", "")
        logger.info(f"Agent executing tool '{tool_name}' with command: {cmd_val}")
        if game_console_enabled() and tool_name == "command" and cmd_val:
            print_game(f"\n> {cmd_val}")
        
        if self.call_delay > 0:
            await asyncio.sleep(self.call_delay)

        tool_timer = Timer.start_new()
        try:
            result = await super().call_tool(tool_name, **kwargs)
            log_kv(
                logger,
                event="tool_call",
                client="ms_agent",
                tool_name=f"mcp.{tool_name}",
                latency_ms=tool_timer.elapsed_ms(),
                args=(format_payload(kwargs) if provider_payload_logging_enabled() else None),
                result=(format_payload(result) if provider_payload_logging_enabled() else None),
            )
            return result
        except Exception as e:
            log_kv(
                logger,
                level="error",
                event="tool_call",
                client="ms_agent",
                tool_name=f"mcp.{tool_name}",
                latency_ms=tool_timer.elapsed_ms(),
                args=(format_payload(kwargs) if provider_payload_logging_enabled() else None),
                error=str(e),
            )
            raise

async def run_ms_mcp_agent(level: str, model_name: str, delay: int, max_turns: int):
    logger.info(f"--- Microsoft Agent Framework MCP Client Starting (Model: {model_name}) ---")
    logger.info(f"Enforcing Turn Limit: {max_turns}")
    
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }
    guidance_file = guidance_map.get(level, "data/guidance_full.txt")
    guidance_cfg = load_guidance(guidance_file)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")
    
    # Configure function invocation to limit loops
    # Note: framework expects a dict matching FunctionInvocationConfiguration
    # max_iterations limits how many times the agent can call tools before returning
    func_config = {"max_iterations": max_turns + 1} # +1 for the initial LOOK

    try:
        # 1. Instantiate the client with turn limit configuration
        if "claude" in model_name.lower():
            client = AnthropicClient(model_id=model_name, function_invocation_configuration=func_config)
            client = LoggingChatClient(client, client_name="anthropic", default_model_id=model_name)
        elif "gemini" in model_name.lower():
            api_key = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
            client = OpenAIChatClient(
                model_id=model_name,
                api_key=api_key,
                base_url="https://generativelanguage.googleapis.com/v1beta/openai/",
                function_invocation_configuration=func_config
            )
            client = LoggingChatClient(client, client_name="google", default_model_id=model_name)
        elif "ollama" in model_name.lower():
            clean_model = model_name
            for prefix in ["ollama:", "ollama/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix):]
            client = OllamaChatClient(
                model_id=clean_model,
                host=os.environ.get("OLLAMA_HOST", "http://localhost:11434"),
                function_invocation_configuration=func_config
            )
            client = LoggingChatClient(client, client_name="ollama", default_model_id=clean_model)
        else:
            client = OpenAIChatClient(model_id=model_name, function_invocation_configuration=func_config)
            client = LoggingChatClient(client, client_name="openai", default_model_id=model_name)

        # 2. Use the MCP Tool as an async context manager
        async with DelayedMCPStreamableHTTPTool(
            name="dustwood-mcp", 
            url=MCP_URL, 
            delay=delay,
            parse_tool_results=mcp_result_parser
        ) as mcp_tool:
            
            # 3. Instantiate the agent with the MCP tool
            guidance_block = f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}" if guidance_cfg.text else ""
            agent = Agent(
                client=client,
                name="DustwoodMCPAdventurer",
                instructions=(
                    "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
                    "Use the 'command' tool to interact with the game world.\n"
                    "The tool returns narrative text and structured game state.\n"
                    "Analyze the state (inventory, thirst, room) to make survival decisions.\n"
                    "Your goal is to survive, explore, and increase your score."
                    f"{guidance_block}"
                ),
                tools=[mcp_tool]
            )
            
            # 4. Start the interaction
            prompt = (
                f"Start by calling the 'command' tool with command='LOOK' and reset=True. "
                f"Then continue playing for exactly {max_turns} more turns. "
                f"IMPORTANT: You have a hard budget of {max_turns} actions. Make them count."
            )
            
            logger.info("\n[STARTING AGENT SESSION]")
            response = await agent.run(prompt)
            logger.info(f"\n[FINAL AGENT RESPONSE]\n{response.text}")

    except Exception as e:
        logger.error(f"Error running agent: {e}")

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS
    
    asyncio.run(run_ms_mcp_agent(level, model, delay, max_turns))
