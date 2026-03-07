import os
import asyncio
import logging
import sys
import httpx
import json
from typing import List, Optional
from pydantic import BaseModel
from dotenv import load_dotenv

from agent_framework import Agent
from agent_framework.openai import OpenAIChatClient

# Load environment variables
load_dotenv()

# --- Configuration ---
MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
DEFAULT_MODEL = "gpt-5-mini"

# --- Setup Logging ---
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)

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

# --- MCP Tool Implementation ---

# Global session state
SESSION_ID: Optional[str] = None

async def execute_mcp_command(command: str, reset: bool = False) -> str:
    """
    Sends a text command to the game via the MCP server and returns the narrative output plus state.
    Use this tool to interact with the game world.
    
    Args:
        command: The action you want to take (e.g., 'NORTH', 'TAKE CANTEEN', 'LOOK')
        reset: Whether to reset the game state (defaults to False)
    """
    global SESSION_ID
    logger.info(f"Agent executing command: {command}")
    
    async with httpx.AsyncClient(timeout=10.0) as client:
        # Initialize session if needed
        if SESSION_ID is None:
            init_payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": {"name": "ms-agent-mcp", "version": "1.0"}
                }
            }
            resp = await client.post(MCP_URL, json=init_payload)
            resp.raise_for_status()
            SESSION_ID = resp.headers.get("Mcp-Session-Id")
            
            # Send initialized notification
            notify_payload = {
                "jsonrpc": "2.0",
                "method": "notifications/initialized"
            }
            headers = {"Mcp-Session-Id": SESSION_ID} if SESSION_ID else {}
            await client.post(MCP_URL, json=notify_payload, headers=headers)

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
        
        headers = {"Mcp-Session-Id": SESSION_ID} if SESSION_ID else {}
        response = await client.post(MCP_URL, json=payload, headers=headers)
        response.raise_for_status()
        data = response.json()
        
    if "error" in data:
        raise RuntimeError(f"MCP Tool Error: {data['error']}")
        
    result_data = data.get("result", {}).get("structuredContent", {})
    if not result_data:
        # Fallback if structuredContent is not available
        content = data.get("result", {}).get("content", [])
        return str(content[0].get("text", "")) if content else "No output"

    result = CommandOutput(**result_data)
    state = result.state
    
    # Format a summary for the agent
    summary_str = (
        f"--- Game State ---\n"
        f"Room: {state.room_name} (ID: {state.room_id})\n"
        f"Turns: {state.turns}, Score: {state.score}, Thirst: {state.thirst}/20\n"
        f"Inventory: {', '.join(state.inventory) if state.inventory else 'Empty'}\n"
        f"Status: Riding={state.is_riding}, Saddled={state.horse_saddled}, Water={state.has_water}\n"
        f"-----------------\n\n"
        f"{result.output}"
    )
    logger.info(f"Game output received (State: {state.room_name})")
    return summary_str

async def run_ms_mcp_agent(model_name: str, goal: str):
    logger.info(f"--- Microsoft Agent Framework MCP Client Starting (Model: {model_name}) ---")
    logger.info(f"Goal: {goal}")
    
    try:
        # Initial look
        initial_summary = await execute_mcp_command("LOOK", reset=True)
        logger.info(f"\n[STARTING GAME]\n{initial_summary}")
        
        # Instantiate the client
        client = OpenAIChatClient(model_id=model_name)
        
        # Instantiate the agent
        agent = Agent(
            client=client,
            name="DustwoodMCPAdventurer",
            instructions=(
                "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
                "You interact with the game via the 'execute_mcp_command' tool.\n"
                "Analyze the structured state provided in the tool output to make optimal choices.\n"
                "Your goal is to survive, explore, and increase your score.\n"
                "When you receive game output, decide on the next command and call 'execute_mcp_command'."
            ),
            tools=[execute_mcp_command]
        )
        
        # Start the interaction loop
        prompt = f"GOAL: {goal}. \nSTARTING STATE: {initial_summary}\nWhat is your first command?"
        
        response = await agent.run(prompt)
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{response.text}")

    except Exception as e:
        logger.error(f"Error running agent: {e}")

if __name__ == "__main__":
    model = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_MODEL
    goal = sys.argv[2] if len(sys.argv) > 2 else "Find the general store and get some water."
    
    asyncio.run(run_ms_mcp_agent(model, goal))
