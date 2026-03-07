import os
import asyncio
import logging
import sys
import argparse
import httpx
import json
from typing import List, Optional
from pydantic import BaseModel, Field
from dotenv import load_dotenv
from pydantic_ai import Agent, RunContext
from pydantic_ai.models import KnownModelName

from guidance_loader import load_guidance

# Load environment variables
load_dotenv()

# --- Configuration ---
MCP_URL = "http://127.0.0.1:8765/mcp"
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-3-flash-preview"
MESSAGE_HISTORY_LIMIT = 5
DEFAULT_GUIDANCE = os.environ.get("GUIDANCE_FILE")

# --- Setup Logging ---
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)

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

# --- Dependencies ---

class MCPDeps:
    def __init__(self, mcp_url: str):
        self.mcp_url = mcp_url
        self.client = httpx.AsyncClient(timeout=10.0)
        self.session_id: Optional[str] = None

    async def execute_command(self, command: str, reset: bool = False) -> CommandOutput:
        """Sends a JSON-RPC request to the MCP server's 'command' tool."""
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
            resp = await self.client.post(self.mcp_url, json=init_payload)
            resp.raise_for_status()
            self.session_id = resp.headers.get("Mcp-Session-Id")
            
            # Send initialized notification
            notify_payload = {
                "jsonrpc": "2.0",
                "method": "notifications/initialized"
            }
            headers = {"Mcp-Session-Id": self.session_id} if self.session_id else {}
            await self.client.post(self.mcp_url, json=notify_payload, headers=headers)

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
        response = await self.client.post(self.mcp_url, json=payload, headers=headers)
        response.raise_for_status()
        data = response.json()
        
        if "error" in data:
            raise RuntimeError(f"MCP Tool Error: {data['error']}")
            
        result = data.get("result", {}).get("structuredContent", {})
        return CommandOutput(**result)

async def run_pydantic_agent(model_name: str, goal: str, guidance: Optional[str] = None):
    deps = MCPDeps(MCP_URL)
    
    logger.info(f"--- Pydantic AI MCP Agent Starting (Model: {model_name}) ---")
    logger.info(f"Goal: {goal}")
    
    try:
        guidance_cfg = load_guidance(guidance)
        if guidance_cfg.path:
            logger.info(f"Guidance: {guidance_cfg.path}")

        guidance_block = f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}" if guidance_cfg.text else ""

        agent = Agent(
            model=model_name,
            deps_type=MCPDeps,
            system_prompt=(
                "You are an expert adventurer playing 'Echoes of Dustwood'.\n"
                "You interact with the game via the 'command' tool.\n"
                "Analyze the structured state to make optimal survival choices.\n"
                "Your goal is to survive, explore, and increase your score."
                f"{guidance_block}"
            ),
        )

        @agent.tool
        async def play_command(ctx: RunContext[MCPDeps], command: str) -> str:
            """Sends a text command to the game and returns the narrative output plus a state summary."""
            result = await ctx.deps.execute_command(command)

            state = result.state
            summary_str = (
                f"--- Game State ---\n"
                f"Room: {state.room_name} (ID: {state.room_id})\n"
                f"Turns: {state.turns}, Score: {state.score}, Thirst: {state.thirst}/20\n"
                f"Inventory: {', '.join(state.inventory) if state.inventory else 'Empty'}\n"
                f"Status: Riding={state.is_riding}, Saddled={state.horse_saddled}, Water={state.has_water}\n"
                f"-----------------\n\n"
                f"{result.output}"
            )
            return summary_str

        # Initial look
        initial = await deps.execute_command("LOOK", reset=True)
        logger.info(f"\n[STARTING GAME]\n{initial.output}")
        
        # Start the interaction loop
        result = await agent.run(
            f"GOAL: {goal}.\n\nSTARTING STATE:\n{initial.output}",
            deps=deps
        )
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{result.output}")
    finally:
        await deps.client.aclose()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Pydantic AI MCP client for Echoes of Dustwood.")
    parser.add_argument("model", nargs="?", default=DEFAULT_MODEL, help="Model name.")
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

    asyncio.run(run_pydantic_agent(args.model, args.goal, args.guidance))
