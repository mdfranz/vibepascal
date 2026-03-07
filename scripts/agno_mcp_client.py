import os
import asyncio
import logging
import sys
import argparse
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
MAX_STEPS = int(os.environ.get("MAX_STEPS", "8"))
DEFAULT_GUIDANCE = os.environ.get("GUIDANCE_FILE")

# --- Setup Logging ---
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)
logging.getLogger("httpx").setLevel(logging.WARNING)
logging.getLogger("httpcore").setLevel(logging.WARNING)
logging.getLogger("mcp").setLevel(logging.WARNING)

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

async def run_agno_mcp_agent(model_name: str, goal: str, guidance: Optional[str] = None):
    logger.info(f"--- Agno MCP Client Starting (Model: {model_name}) ---")
    logger.info(f"Goal: {goal}")
    guidance_cfg = load_guidance(guidance)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")
    
    try:
        # Agno MCP integration (Streamable HTTP)
        async with MCPTools(url=MCP_URL, transport="streamable-http") as mcp_tools:
            last_state: Optional[GameSummary] = None

            async def command(command: str, reset: bool = False) -> str:
                """Send a game command via MCP and return narrative output plus a structured state summary."""
                nonlocal last_state
                logger.info(f"Agent executing command: {command}")
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
            agent = Agent(
                model=model,
                name="DustwoodAgnoMCPAdventurer",
                description=(
                    "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
                    "You must choose the next game command to execute.\n"
                    "Only output a single game command per step (one line, no extra text).\n"
                    "Prefer standard parser commands like LOOK, INVENTORY, N/S/E/W, TAKE <item>, USE <item>.\n"
                    "Your goal is to survive, explore, and increase your score."
                ),
                markdown=True,
            )
        
            # Bounded interaction loop (prevents runaway tool-calling)
            current_summary = initial_summary
            for step_idx in range(1, MAX_STEPS + 1):
                if last_state is not None and not last_state.is_playing:
                    logger.info("\n[GAME OVER]")
                    break

                guidance_block = ""
                if guidance_cfg.text:
                    guidance_block = f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}"

                prompt = (
                    f"GOAL: {goal}\n\n"
                    f"CURRENT STATE:\n{current_summary}\n\n"
                    f"Step {step_idx}/{MAX_STEPS}: Output exactly one next game command (one line)."
                    f"\nRules: do not repeat LOOK unless state changed; prefer movement/exploration when safe."
                    f"{guidance_block}"
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
    parser = argparse.ArgumentParser(description="Agno MCP client for Echoes of Dustwood.")
    parser.add_argument("model", nargs="?", default=DEFAULT_MODEL, help="Model name (default: gpt-5-mini).")
    parser.add_argument(
        "goal",
        nargs="?",
        default="Find the general store and get some water.",
        help="Goal for the agent.",
    )
    parser.add_argument(
        "--guidance",
        default=DEFAULT_GUIDANCE,
        help="Guidance file path or level (full|medium|minimal). Can also be set via GUIDANCE_FILE.",
    )
    args = parser.parse_args()

    asyncio.run(run_agno_mcp_agent(args.model, args.goal, args.guidance))
