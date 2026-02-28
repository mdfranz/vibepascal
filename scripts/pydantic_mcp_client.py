import os
import asyncio
import logging
import sys
import httpx
import json
from typing import List, Optional
from pydantic import BaseModel, Field
from dotenv import load_dotenv
from pydantic_ai import Agent, RunContext
from pydantic_ai.models import KnownModelName

# Load environment variables
load_dotenv()

# --- Configuration ---
MCP_URL = "http://127.0.0.1:8765/mcp"
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-3-flash-preview"
MESSAGE_HISTORY_LIMIT = 5

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

    async def execute_command(self, command: str, reset: bool = False) -> CommandOutput:
        """Sends a JSON-RPC request to the MCP server's 'command' tool."""
        payload = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/call",
            "params": {
                "name": "command",
                "arguments": {
                    "command": command,
                    "reset": reset
                }
            }
        }
        
        response = await self.client.post(self.mcp_url, json=payload)
        response.raise_for_status()
        data = response.json()
        
        if "error" in data:
            raise RuntimeError(f"MCP Tool Error: {data['error']}")
            
        result = data.get("result", {}).get("structuredContent", {})
        return CommandOutput(**result)

# --- Agent Definition ---

agent = Agent(
    model=DEFAULT_MODEL,
    deps_type=MCPDeps,
    system_prompt=(
        """You are an expert adventurer playing 'Echoes of Dustwood'.
You interact with the game via the 'command' tool.
Analyze the structured state to make optimal survival choices.
Your goal is to survive, explore, and increase your score."""
    )
)

@agent.tool
async def play_command(ctx: RunContext[MCPDeps], command: str) -> str:
    """Sends a text command to the game and returns the narrative output plus a state summary."""
    result = await ctx.deps.execute_command(command)
    
    # We return a formatted string that includes both flavor text and critical state
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

async def run_pydantic_agent(model_name: str, goal: str):
    deps = MCPDeps(MCP_URL)
    
    logger.info(f"--- Pydantic AI MCP Agent Starting (Model: {model_name}) ---")
    logger.info(f"Goal: {goal}")
    
    try:
        # Initial look
        initial = await deps.execute_command("LOOK", reset=True)
        logger.info(f"\n[STARTING GAME]\n{initial.output}")
        
        # Start the interaction loop
        result = await agent.run(
            f"GOAL: {goal}. \nSTARTING STATE: {initial.output}", 
            deps=deps
        )
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{result.output}")
    finally:
        await deps.client.aclose()

if __name__ == "__main__":
    model = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_MODEL
    goal = sys.argv[2] if len(sys.argv) > 2 else "Find the general store and get some water."
    
    asyncio.run(run_pydantic_agent(model, goal))
