import os
import asyncio
import logging
import sys
import time
from typing import List, Optional
from pydantic import BaseModel
from dotenv import load_dotenv

from agno.agent import Agent
from agno.models.openai import OpenAIChat
from agno.models.anthropic import Claude
from agno.models.google import Gemini
from agno.models.ollama import Ollama
from agno.tools.mcp import MCPTools

from guidance_loader import load_guidance

# Load environment variables
load_dotenv()

# --- Configuration ---
MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
DEFAULT_MODEL = "gpt-5-mini"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/agno_mcp_client-{EPOCH}.log"

# --- Setup Logging ---
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
os.makedirs("logs", exist_ok=True)

file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s [%(levelname)s] %(message)s'))
logger.addHandler(file_handler)

console_handler = logging.StreamHandler()
console_handler.setLevel(logging.INFO)
console_handler.setFormatter(logging.Formatter('%(message)s'))
logger.addHandler(console_handler)

# Silence verbose loggers
logging.getLogger("httpx").setLevel(logging.WARNING)
logging.getLogger("httpcore").setLevel(logging.WARNING)
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

def _format_command_result(*, structured_content: dict) -> str:
    result = CommandOutput(**structured_content)
    state = result.state
    logger.info(f"Game output received (State: {state.room_name})")
    return (
        f"--- Game State ---\n"
        f"Room: {state.room_name} (ID: {state.room_id})\n"
        f"Turns: {state.turns}, Score: {state.score}, Playing: {state.is_playing}, Thirst: {state.thirst}/20\n"
        f"Inventory: {', '.join(state.inventory) if state.inventory else 'Empty'}\n"
        f"Status: Riding={state.is_riding}, Saddled={state.horse_saddled}, Water={state.has_water}\n"
        f"-----------------\n\n"
        f"{result.output}"
    )

async def run_agno_mcp_agent(level: str, model_name: str, delay: int, max_turns: int):
    logger.info(f"--- Agno MCP Client Starting (Model: {model_name}) ---")
    
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }
    guidance_file = guidance_map.get(level, "data/guidance_full.txt")
    guidance_cfg = load_guidance(guidance_file)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")
    
    global global_delay
    global_delay = delay

    try:
        # Agno MCP integration (Streamable HTTP)
        async with MCPTools(url=MCP_URL, transport="streamable-http") as mcp_tools:
            last_state: Optional[GameSummary] = None

            async def command(command: str, reset: bool = False) -> str:
                """Send a game command via MCP and return narrative output plus a structured state summary."""
                nonlocal last_state
                logger.info(f"Agent executing command: {command}")
                
                if global_delay > 0 and not reset:
                    await asyncio.sleep(global_delay)

                result = await mcp_tools.session.call_tool(  # type: ignore[union-attr]
                    "command",
                    {"command": command, "reset": reset},
                )
                if result.isError:
                    raise RuntimeError("MCP tool 'command' returned an error.")

                if result.structuredContent:
                    parsed = CommandOutput(**result.structuredContent)
                    last_state = parsed.state
                    return _format_command_result(structured_content=result.structuredContent)

                text_parts = [getattr(c, "text", "") for c in (result.content or [])]
                return next((t for t in text_parts if t), "No output")

            # Initial look + reset
            initial_summary = await command("LOOK", reset=True)
            logger.info(f"\n[STARTING GAME]\n{initial_summary}")

            # Instantiate the model
            if "claude" in model_name.lower():
                model = Claude(id=model_name)
            elif "gemini" in model_name.lower():
                model = Gemini(id=model_name)
            elif "ollama" in model_name.lower():
                clean_model = model_name
                for prefix in ["ollama:", "ollama/"]:
                    if clean_model.lower().startswith(prefix):
                        clean_model = clean_model[len(prefix) :]
                model = Ollama(id=clean_model, host=os.environ.get("OLLAMA_HOST", "http://localhost:11434"))
            else:
                model = OpenAIChat(id=model_name)
        
            # Instantiate the agent
            guidance_block = f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}" if guidance_cfg.text else ""
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
            )
        
            # Bounded interaction loop
            current_summary = initial_summary
            for step_idx in range(1, max_turns + 1):
                if last_state is not None and not last_state.is_playing:
                    logger.info("\n[GAME OVER]")
                    break

                prompt = (
                    f"CURRENT STATE:\n{current_summary}\n\n"
                    f"Step {step_idx}/{max_turns}: Output exactly one next game command (one line)."
                    f"\nRules: do not repeat LOOK unless state changed; prefer movement/exploration when safe."
                )
                run_output = await agent.arun(prompt)
                next_cmd = (run_output.content or "").strip()
                next_cmd = next_cmd.splitlines()[0].strip().strip("`").strip()
                if not next_cmd:
                    raise RuntimeError("Agent produced an empty command.")

                current_summary = await command(next_cmd)

            logger.info(f"\n[FINAL STATE]\n{current_summary}")

    except Exception as e:
        logger.error(f"Error running agent: {e}")

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS
    
    asyncio.run(run_agno_mcp_agent(level, model, delay, max_turns))
