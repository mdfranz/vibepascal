import asyncio
import json
import logging
import os
import sys
import time
from typing import Any, List, Optional

from agent_framework import Agent
from agent_framework._mcp import (
    MCPStreamableHTTPTool,  # Using the official MCP tool integration
)
from agent_framework.anthropic import AnthropicClient
from agent_framework.ollama import OllamaChatClient
from agent_framework.openai import OpenAIChatClient
from dotenv import load_dotenv
from guidance_loader import load_guidance
from llm_observability import (
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
from mcp_command_policy import CommandPolicy, sanitize_command
from pydantic import BaseModel

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
file_handler.setFormatter(logging.Formatter("%(asctime)s [%(levelname)s] %(message)s"))
logger.addHandler(file_handler)

if console_logging_enabled():
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)
    console_handler.setFormatter(logging.Formatter("%(message)s"))
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


def _trim_output(text: str, max_chars: int = 1200) -> str:
    s = (text or "").strip()
    if len(s) <= max_chars:
        return s
    return s[-max_chars:]


def _serialize_messages(messages: Any) -> Any:
    try:
        return [
            getattr(m, "to_dict")() if hasattr(m, "to_dict") else str(m)
            for m in (messages or [])
        ]
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

    def get_response(
        self, messages: Any, *, stream: bool = False, options: Any = None, **kwargs: Any
    ):
        request_timer = Timer.start_new()
        request_payload = (
            _serialize_messages(messages)
            if provider_payload_logging_enabled()
            else None
        )

        try:
            inner_result = self._inner.get_response(
                messages, stream=stream, options=options, **kwargs
            )
        except Exception as e:
            log_kv(
                logger,
                level="error",
                event="provider_call",
                client="ms_agent",
                provider=self._client_name,
                model=self._default_model_id,
                latency_ms=request_timer.elapsed_ms(),
                request=(
                    format_payload(request_payload)
                    if request_payload is not None
                    else None
                ),
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
                    model=getattr(final_response, "model_id", None)
                    or self._default_model_id,
                    latency_ms=request_timer.elapsed_ms(),
                    usage=(
                        format_payload(usage_details)
                        if (
                            usage_details is not None
                            and provider_payload_logging_enabled()
                        )
                        else None
                    ),
                    tool_call_count=len(tool_calls) if tool_calls else 0,
                    tool_calls=(
                        format_payload(tool_calls)
                        if (tool_calls and provider_payload_logging_enabled())
                        else None
                    ),
                    request=(
                        format_payload(request_payload)
                        if request_payload is not None
                        else None
                    ),
                    response=(
                        format_payload(getattr(final_response, "text", None))
                        if provider_payload_logging_enabled()
                        else None
                    ),
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
                    request=(
                        format_payload(request_payload)
                        if request_payload is not None
                        else None
                    ),
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
                model=getattr(final_response, "model_id", None)
                or self._default_model_id,
                latency_ms=request_timer.elapsed_ms(),
                usage=(
                    format_payload(usage_details)
                    if (
                        usage_details is not None and provider_payload_logging_enabled()
                    )
                    else None
                ),
                tool_call_count=len(tool_calls) if tool_calls else 0,
                tool_calls=(
                    format_payload(tool_calls)
                    if (tool_calls and provider_payload_logging_enabled())
                    else None
                ),
                request=(
                    format_payload(request_payload)
                    if request_payload is not None
                    else None
                ),
                response=(
                    format_payload(getattr(final_response, "text", None))
                    if provider_payload_logging_enabled()
                    else None
                ),
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
                args=(
                    format_payload(kwargs)
                    if provider_payload_logging_enabled()
                    else None
                ),
                result=(
                    format_payload(result)
                    if provider_payload_logging_enabled()
                    else None
                ),
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
                args=(
                    format_payload(kwargs)
                    if provider_payload_logging_enabled()
                    else None
                ),
                error=str(e),
            )
            raise


class PolicyMCPTool(DelayedMCPStreamableHTTPTool):
    """MCP tool wrapper that enforces turn budgeting + loop breaking at the tool boundary."""

    def __init__(
        self,
        *args: Any,
        policy: CommandPolicy,
        max_turns: int,
        on_step: Any | None = None,
        **kwargs: Any,
    ):
        self._policy = policy
        self._max_turns = int(max_turns)
        self._on_step = on_step
        self.last_state: GameSummary | None = None
        self.last_output_text: str = ""
        self._pending_command: str = ""
        kwargs["parse_tool_results"] = self._parse_tool_results
        super().__init__(*args, **kwargs)

    def _parse_tool_results(self, result: Any) -> str:
        if hasattr(result, "structuredContent") and result.structuredContent:
            try:
                parsed = CommandOutput(**result.structuredContent)
                self.last_state = parsed.state
                self.last_output_text = parsed.output
                if self._pending_command:
                    self._policy.observe(
                        command=self._pending_command,
                        state=self.last_state,
                        output_text=self.last_output_text,
                    )
                    if self._on_step is not None:
                        try:
                            self._on_step(
                                self._pending_command,
                                self.last_state,
                                self.last_output_text,
                            )
                        except Exception:
                            pass
                st = parsed.state
                logger.info(f"Game output received (State: {st.room_name})")
                if game_console_enabled():
                    print_game(
                        f"\n[turn={st.turns} room={st.room_name} score={st.score} thirst={st.thirst}]\n"
                        f"{parsed.output.strip()}\n"
                    )
                return (
                    f"--- Game State ---\n"
                    f"Room: {st.room_name} (ID: {st.room_id})\n"
                    f"Turns: {st.turns}, Score: {st.score}, Thirst: {st.thirst}\n"
                    f"Inventory: {', '.join(st.inventory) if st.inventory else 'Empty'}\n"
                    f"Status: Riding={st.is_riding}, Saddled={st.horse_saddled}, Water={st.has_water}\n"
                    f"-----------------\n\n"
                    f"{_trim_output(parsed.output)}"
                )
            except Exception as e:
                logger.warning(f"Failed to parse structuredContent: {e}")

        if hasattr(result, "content"):
            text = "\n".join([c.text for c in result.content if hasattr(c, "text")])
            self.last_output_text = text
            return text

        self.last_output_text = str(result)
        return self.last_output_text

    async def call_tool(self, tool_name: str, **kwargs: Any) -> str:
        if tool_name == "command":
            reset = bool(kwargs.get("reset", False))
            if (
                self.last_state is not None
                and not reset
                and self.last_state.turns >= self._max_turns
            ):
                return "Turn limit reached."
            raw = sanitize_command(str(kwargs.get("command", "")))
            if self.last_state is not None:
                rewritten = self._policy.rewrite(
                    proposed_command=raw,
                    state=self.last_state,
                    max_turns=self._max_turns,
                )
            else:
                rewritten = raw or "LOOK"
            self._pending_command = rewritten
            kwargs["command"] = rewritten
        return await super().call_tool(tool_name, **kwargs)


async def run_ms_mcp_agent(level: str, model_name: str, delay: int, max_turns: int):
    logger.info(
        f"--- Microsoft Agent Framework MCP Client Starting (Model: {model_name}) ---"
    )
    logger.info(f"Enforcing Turn Limit: {max_turns}")

    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt",
    }
    guidance_file = guidance_map.get(level, "data/guidance_full.txt")
    guidance_cfg = load_guidance(guidance_file)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")

    policy = CommandPolicy.from_env()
    max_llm_calls = max(1, int(max_turns) * int(policy.max_llm_calls_multiplier))
    llm_calls = 0

    try:
        # 1. Instantiate the client with turn limit configuration
        if "claude" in model_name.lower():
            client = AnthropicClient(model_id=model_name)
            client = LoggingChatClient(
                client, client_name="anthropic", default_model_id=model_name
            )
        elif "gemini" in model_name.lower():
            api_key = os.environ.get("GEMINI_API_KEY") or os.environ.get(
                "GOOGLE_API_KEY"
            )
            client = OpenAIChatClient(
                model_id=model_name,
                api_key=api_key,
                base_url="https://generativelanguage.googleapis.com/v1beta/openai/",
            )
            client = LoggingChatClient(
                client, client_name="google", default_model_id=model_name
            )
        elif "ollama" in model_name.lower():
            clean_model = model_name
            for prefix in ["ollama:", "ollama/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix) :]
            client = OllamaChatClient(
                model_id=clean_model,
                host=os.environ.get("OLLAMA_HOST", "http://localhost:11434"),
            )
            client = LoggingChatClient(
                client, client_name="ollama", default_model_id=clean_model
            )
        else:
            client = OpenAIChatClient(model_id=model_name)
            client = LoggingChatClient(
                client, client_name="openai", default_model_id=model_name
            )

        # 2. Use the MCP Tool as an async context manager
        history: list[str] = []

        def _on_step(command: str, st: GameSummary, output_text: str) -> None:
            history.append(
                f"t={st.turns} cmd={command} room={st.room_name} score={st.score} thirst={st.thirst}\n"
                f"{_trim_output(output_text, max_chars=500)}"
            )
            if len(history) > policy.history_limit:
                del history[: -policy.history_limit]

        async with PolicyMCPTool(
            name="dustwood-mcp",
            url=MCP_URL,
            delay=delay,
            policy=policy,
            max_turns=max_turns,
            on_step=_on_step,
        ) as mcp_tool:
            # 3. Instantiate the agent with the MCP tool
            guidance_block = (
                f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}"
                if guidance_cfg.text
                else ""
            )
            agent = Agent(
                client=client,
                name="DustwoodMCPAdventurer",
                instructions=(
                    "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
                    "Use the 'command' tool to interact with the game world.\n"
                    "The tool returns narrative text and structured game state.\n"
                    "Analyze the state (inventory, thirst, room) to make survival decisions.\n"
                    "LOOK does not consume a game turn; do not repeat LOOK if turns did not change.\n"
                    "Exits may not be listed; try NORTH/EAST/SOUTH/WEST to explore when unsure.\n"
                    "Your goal is to survive, explore, and increase your score."
                    f"{guidance_block}"
                ),
                tools=[mcp_tool],
                default_options={"allow_multiple_tool_calls": False},
            )

            # 4. Hybrid replanning loop (small tool-call chunks)
            logger.info("\n[STARTING AGENT SESSION]")
            await mcp_tool.call_tool("command", command="LOOK", reset=True)

            replans = 0
            last_turns_seen = (
                mcp_tool.last_state.turns if mcp_tool.last_state is not None else 0
            )

            while (
                mcp_tool.last_state is not None
                and mcp_tool.last_state.is_playing
                and mcp_tool.last_state.turns < max_turns
                and llm_calls < max_llm_calls
            ):
                replans += 1
                remaining = max(0, max_turns - mcp_tool.last_state.turns)
                chunk_calls = min(6, remaining + 2)

                # Limit tool usage per replan.
                inner = getattr(client, "_inner", client)
                try:
                    inner.function_invocation_configuration["max_function_calls"] = int(
                        chunk_calls
                    )
                    inner.function_invocation_configuration["max_iterations"] = 3
                except Exception:
                    pass

                recent_history = (
                    "\n".join(history[-policy.history_limit :]) if history else "(none)"
                )
                state_block = (
                    f"--- Game State ---\n"
                    f"Room: {mcp_tool.last_state.room_name} (ID: {mcp_tool.last_state.room_id})\n"
                    f"Turns: {mcp_tool.last_state.turns}, Score: {mcp_tool.last_state.score}, Thirst: {mcp_tool.last_state.thirst}\n"
                    f"Inventory: {', '.join(mcp_tool.last_state.inventory) if mcp_tool.last_state.inventory else 'Empty'}\n"
                    f"Status: Riding={mcp_tool.last_state.is_riding}, Saddled={mcp_tool.last_state.horse_saddled}, Water={mcp_tool.last_state.has_water}\n"
                    f"-----------------\n\n"
                    f"{_trim_output(mcp_tool.last_output_text)}"
                )
                prompt = (
                    f"RECENT HISTORY (most recent last):\n{recent_history}\n\n"
                    f"CURRENT STATE:\n{state_block}\n\n"
                    f"Remaining game turns: {remaining}\n"
                    f"Play efficiently. Use the 'command' tool to make moves. "
                    f"Do NOT waste turns on LOOK/INVENTORY unless necessary. "
                    f"Stop calling tools when you have used about {chunk_calls} tool calls or when the game ends."
                )
                llm_calls += 1
                response = await agent.run(prompt)
                logger.info(f"\n[REPLAN {replans} RESPONSE]\n{response.text}")

                if mcp_tool.last_state is None or not mcp_tool.last_state.is_playing:
                    break

                # If the model didn't advance turns, force a deterministic exploratory move.
                if mcp_tool.last_state.turns == last_turns_seen:
                    forced = policy.rewrite(
                        proposed_command="LOOK",
                        state=mcp_tool.last_state,
                        max_turns=max_turns,
                    )
                    await mcp_tool.call_tool("command", command=forced, reset=False)

                last_turns_seen = mcp_tool.last_state.turns

            final_text = (
                f"Final: turns={mcp_tool.last_state.turns if mcp_tool.last_state else 'n/a'} "
                f"score={mcp_tool.last_state.score if mcp_tool.last_state else 'n/a'} "
                f"room={mcp_tool.last_state.room_name if mcp_tool.last_state else 'n/a'}"
            )
            logger.info(f"\n[FINAL]\n{final_text}")

    except Exception as e:
        logger.error(f"Error running agent: {e}")


if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS

    asyncio.run(run_ms_mcp_agent(level, model, delay, max_turns))
