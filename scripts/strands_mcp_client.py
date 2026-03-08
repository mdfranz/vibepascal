import os
import asyncio
import logging
import sys
from typing import Optional
from dotenv import load_dotenv

# Strands Imports
from strands import Agent
from strands.models.litellm import LiteLLMModel
from strands.agent.conversation_manager import SlidingWindowConversationManager
from strands.tools.mcp import MCPClient
from mcp import stdio_client, StdioServerParameters

# Load environment variables
load_dotenv()

# --- Configuration ---
# Use the binary directly for stdio transport (no --mcp-http)
# We assume the binary handles stdio MCP if run without flags or with specific flags
# Based on src/golang/main.go, it needs a way to trigger MCP on stdio.
# Actually, the Go SDK usually handles this if we use a specific entry point.
# Let's assume the current binary supports it or we need to add it.
# If the binary ONLY supports HTTP, we should use the HTTP transport in Strands.

STDIO_PARAMS = StdioServerParameters(
    command="./bin/dustwood-go",
    args=["--turns", "1000"] # We might need a --mcp-stdio flag if we implemented one
)
DEFAULT_MODEL_ID = "gemini/gemini-3-flash-preview"
MESSAGE_HISTORY_LIMIT = 10

# --- Setup Logging ---
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)

async def run_strands_agent(model_id: str, goal: str):
    # 1. Initialize the LLM
    llm_model = LiteLLMModel(
        model_id=model_id,
        params={"max_tokens": 4000}
    )

    # 2. Initialize the MCP Client
    # If the Go server is running via HTTP, let's use the SSE client which is more standard for Strands
    from mcp.client.sse import sse_client
    MCP_URL = "http://127.0.0.1:8765/mcp"
    mcp_client = MCPClient(lambda: sse_client(MCP_URL))

    # 3. Setup Conversation Manager
    conv_manager = SlidingWindowConversationManager(window_size=MESSAGE_HISTORY_LIMIT)

    # 4. Initialize Agent with MCP Tools
    agent = Agent(
        model=llm_model,
        system_prompt=(
            "You are an expert text adventure player. Your goal is to play 'Echoes of Dustwood'.\n"
            "Use the 'command' tool to interact with the game. \n"
            "The tool returns both the narrative text and a structured game state.\n"
            "Analyze the state (inventory, thirst, room) to make survival decisions.\n"
            "Always try to survive and increase your score."
        ),
        tools=[mcp_client],
        conversation_manager=conv_manager
    )

    logger.info(f"--- Strands MCP Agent Starting (Model: {model_id}) ---")
    logger.info(f"Goal: {goal}")

    try:
        # Start the agent
        result = await agent.run(goal)
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{result}")
    except Exception as e:
        logger.error(f"Error during agent execution: {e}")

if __name__ == "__main__":
    model = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_MODEL_ID
    goal = sys.argv[2] if len(sys.argv) > 2 else "Perform a LOOK command to see where we are, then stop."
    
    asyncio.run(run_strands_agent(model, goal))
