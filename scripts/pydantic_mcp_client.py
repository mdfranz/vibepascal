import os
import asyncio
import logging
import sys
import httpx
import json
import time
import re
from typing import List, Optional
from pydantic import BaseModel, Field
from dotenv import load_dotenv
from pydantic_ai import Agent, RunContext
from pydantic_ai.models import KnownModelName

from guidance_loader import load_guidance
from mcp_command_policy import CommandPolicy, sanitize_command
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
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-3-flash-preview"
MESSAGE_HISTORY_LIMIT = 5
TURN_DELAY = 1
MAX_TURNS = 25
MAX_OUTPUT_CHARS = 1200

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/pydantic_mcp_client-{EPOCH}.log"

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

# Global variable for delay
global_delay = TURN_DELAY

# --- State Models (Matching Go Summary) ---

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

# --- Helper Logic ---
def _trim_output(text: str, max_chars: int = MAX_OUTPUT_CHARS) -> str:
    s = (text or "").strip()
    if len(s) <= max_chars:
        return s
    return s[-max_chars:]

# --- Dependencies ---

class MCPDeps:
    def __init__(self, mcp_url: str):
        self.mcp_url = mcp_url
        self.client = httpx.AsyncClient(timeout=10.0)
        self.session_id: Optional[str] = None

    async def execute_command(self, command: str, reset: bool = False) -> CommandOutput:
        """Sends a JSON-RPC request to the MCP server's 'command' tool."""
        if game_console_enabled():
            print_game(f"\n> {command}")
        
        if global_delay > 0 and not reset:
            await asyncio.sleep(global_delay)

        # Initialize session if needed
        if self.session_id is None:
            init_payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": {"name": "pydantic-ai-mcp", "version": "1.0"}
                }
            }
            init_timer = Timer.start_new()
            resp = await self.client.post(self.mcp_url, json=init_payload)
            resp.raise_for_status()
            self.session_id = resp.headers.get("Mcp-Session-Id")
            log_kv(
                logger,
                event="tool_call",
                client="pydantic_ai",
                tool_name="mcp.initialize",
                latency_ms=init_timer.elapsed_ms(),
                args=(format_payload(init_payload) if provider_payload_logging_enabled() else None),
                result=(format_payload({"status_code": resp.status_code, "session_id": self.session_id}) if provider_payload_logging_enabled() else None),
            )
            
            # Send initialized notification
            notify_payload = {
                "jsonrpc": "2.0",
                "method": "notifications/initialized"
            }
            headers = {"Mcp-Session-Id": self.session_id} if self.session_id else {}
            notify_timer = Timer.start_new()
            notify_resp = await self.client.post(self.mcp_url, json=notify_payload, headers=headers)
            notify_resp.raise_for_status()
            log_kv(
                logger,
                event="tool_call",
                client="pydantic_ai",
                tool_name="mcp.notifications/initialized",
                latency_ms=notify_timer.elapsed_ms(),
                args=(format_payload(notify_payload) if provider_payload_logging_enabled() else None),
                result=(format_payload({"status_code": notify_resp.status_code}) if provider_payload_logging_enabled() else None),
            )

        payload = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "command",
                "arguments": {
                    "command": command,
                    "reset": reset
                }
            }
        }
        
        headers = {"Mcp-Session-Id": self.session_id} if self.session_id else {}
        tool_timer = Timer.start_new()
        response = await self.client.post(self.mcp_url, json=payload, headers=headers)
        response.raise_for_status()
        data = response.json()
        
        if "error" in data:
            log_kv(
                logger,
                level="error",
                event="tool_call",
                client="pydantic_ai",
                tool_name="mcp.command",
                latency_ms=tool_timer.elapsed_ms(),
                args=(format_payload(payload) if provider_payload_logging_enabled() else None),
                error=format_payload(data.get("error")),
            )
            raise RuntimeError(f"MCP Tool Error: {data['error']}")
            
        result = data.get("result", {}).get("structuredContent", {})
        log_kv(
            logger,
            event="tool_call",
            client="pydantic_ai",
            tool_name="mcp.command",
            latency_ms=tool_timer.elapsed_ms(),
            args=(format_payload(payload) if provider_payload_logging_enabled() else None),
            result=(format_payload(result) if provider_payload_logging_enabled() else None),
        )
        parsed = CommandOutput(**result)
        if game_console_enabled():
            s = parsed.state
            print_game(
                f"\n[turn={s.turns} room={s.room_name} score={s.score} thirst={s.thirst}/20]\n"
                f"{parsed.output.strip()}\n"
            )
        return parsed

async def run_pydantic_agent(level: str, model_name: str, delay: int, max_turns: int):
    deps = MCPDeps(MCP_URL)
    logger.info(f"--- Pydantic AI MCP Agent Starting (Model: {model_name}) ---")
    
    global global_delay
    global_delay = delay

    try:
        policy = CommandPolicy.from_env()
        max_llm_calls = max(1, int(max_turns) * int(policy.max_llm_calls_multiplier))
        llm_calls = 0
        history: list[str] = []

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
            model=model_name,
            deps_type=MCPDeps,
            system_prompt=(
                "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
                "You must choose the next game command to execute.\n"
                "Only output a single game command per step (one line, no extra text).\n"
                "LOOK does not consume a game turn; do not repeat LOOK if turns did not change.\n"
                "Exits may not be listed. If unsure, try a cardinal move (NORTH/EAST/SOUTH/WEST).\n"
                "Prefer standard parser commands like LOOK, INVENTORY, N/S/E/W, TAKE <item>, USE <item>.\n"
                "Your goal is to survive, explore, and increase your score."
                f"{guidance_block}"
            ),
        )

        # Initial look
        initial = await deps.execute_command("LOOK", reset=True)
        logger.info(f"\n[STARTING GAME]\n{initial.output}")
        policy.observe(command="LOOK", state=initial.state, output_text=initial.output)
        
        def _summary(*, res: CommandOutput) -> str:
            st = res.state
            return (
                f"--- Game State ---\n"
                f"Room: {st.room_name} (ID: {st.room_id})\n"
                f"Turns: {st.turns}, Score: {st.score}, Thirst: {st.thirst}/20\n"
                f"Inventory: {', '.join(st.inventory) if st.inventory else 'Empty'}\n"
                f"Status: Riding={st.is_riding}, Saddled={st.horse_saddled}, Water={st.has_water}\n"
                f"-----------------\n\n"
                f"{_trim_output(res.output)}"
            )

        current_res = initial
        current_summary_str = _summary(res=current_res)

        # Bounded interaction loop
        while current_res.state.is_playing and current_res.state.turns < max_turns and llm_calls < max_llm_calls:
            remaining_turns = max(0, max_turns - current_res.state.turns)
            recent_history = "\n".join(history[-MESSAGE_HISTORY_LIMIT:]) if history else "(none)"
            prompt = (
                f"RECENT HISTORY (most recent last):\n{recent_history}\n\n"
                f"CURRENT STATE:\n{current_summary_str}\n\n"
                f"Remaining game turns: {remaining_turns}\n"
                f"Output exactly one next game command (one line)."
            )
            
            provider_timer = Timer.start_new()
            result = await agent.run(prompt, deps=deps)
            llm_calls += 1
            latency_ms = provider_timer.elapsed_ms()
            usage = result.usage()
            tool_calls = []
            try:
                tool_calls = [
                    {"tool_name": tc.tool_name, "args": tc.args}
                    for tc in (result.response.tool_calls() or [])
                ]
            except Exception:
                tool_calls = []
            log_kv(
                logger,
                event="provider_call",
                client="pydantic_ai",
                model=getattr(result.response, "model_name", None) or model_name,
                latency_ms=latency_ms,
                requests=getattr(usage, "requests", None),
                input_tokens=getattr(usage, "input_tokens", None),
                output_tokens=getattr(usage, "output_tokens", None),
                total_tokens=getattr(usage, "total_tokens", None),
                tool_calls=getattr(usage, "tool_calls", None),
                llm_tool_calls=(format_payload(tool_calls) if (tool_calls and provider_payload_logging_enabled()) else None),
                prompt=(format_payload(prompt) if provider_payload_logging_enabled() else None),
                response=(format_payload(getattr(result, "output", None)) if provider_payload_logging_enabled() else None),
            )
            raw_cmd = sanitize_command(str(result.output))
            next_cmd = policy.rewrite(proposed_command=raw_cmd, state=current_res.state, max_turns=max_turns)
            if not next_cmd:
                raise RuntimeError("Agent produced an empty command.")
            
            res = await deps.execute_command(next_cmd)
            policy.observe(command=next_cmd, state=res.state, output_text=res.output)

            history.append(
                f"t={res.state.turns} cmd={next_cmd} room={res.state.room_name} score={res.state.score} thirst={res.state.thirst}/20\n"
                f"{_trim_output(res.output, max_chars=500)}"
            )
            if len(history) > MESSAGE_HISTORY_LIMIT:
                history = history[-MESSAGE_HISTORY_LIMIT:]

            current_res = res
            current_summary_str = _summary(res=current_res)
            
            if not res.state.is_playing or "Final score" in res.output:
                logger.info("\n[GAME ENDED]")
                break

        logger.info(f"\n[FINAL AGENT RESPONSE]\n{current_summary_str}")
    finally:
        await deps.client.aclose()

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS
    
    asyncio.run(run_pydantic_agent(level, model, delay, max_turns))
