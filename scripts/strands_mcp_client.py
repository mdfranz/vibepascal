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

    logger.info(f"--- Strands MCP Agent Starting (Model: {model_id}) ---")
    
    prompt = f"Perform a LOOK command to start, then continue playing for up to {max_turns} turns to increase your score."

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
