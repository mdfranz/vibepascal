import os
import logging
import argparse
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
DEFAULT_GUIDANCE = os.environ.get("GUIDANCE_FILE")

from guidance_loader import load_guidance

# --- Setup Logging ---
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)

def run_strands_agent(
    model_id: str,
    goal: str,
    transport: str = "streamable-http",
    guidance: Optional[str] = None,
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
    guidance_cfg = load_guidance(guidance)
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
    logger.info(f"Goal: {goal}")

    try:
        result = agent(goal)
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{str(result).strip()}")
    except Exception as e:
        logger.error(f"Error during agent execution: {e}")
    finally:
        try:
            mcp_client.stop(None, None, None)
        except Exception:
            pass

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Strands MCP client for Echoes of Dustwood.")
    parser.add_argument("model", nargs="?", default=DEFAULT_MODEL_ID, help="LiteLLM model id.")
    parser.add_argument(
        "goal",
        nargs="?",
        default="Perform a LOOK command to see where we are, then stop.",
        help="Goal for the agent.",
    )
    parser.add_argument(
        "transport",
        nargs="?",
        default="streamable-http",
        help="MCP transport (streamable-http|sse|stdio). Note: dustwood-go with --mcp-json-response requires streamable-http.",
    )
    parser.add_argument(
        "--guidance",
        default=DEFAULT_GUIDANCE,
        help="Guidance file path or level (full|medium|minimal). Can also be set via GUIDANCE_FILE.",
    )
    args = parser.parse_args()

    run_strands_agent(args.model, args.goal, args.transport, args.guidance)
