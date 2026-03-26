import asyncio
import logging
import os
import sys
import time

from dotenv import load_dotenv
from guidance_loader import load_guidance
from llm_observability import (
    Timer,
    console_logging_enabled,
    enable_http_debug_logging,
    http_debug_logging_enabled,
    log_kv,
)
from pydantic_ai import Agent
from pydantic_ai._agent_graph import CallToolsNode
from pydantic_ai.exceptions import UnexpectedModelBehavior, UsageLimitExceeded
from pydantic_ai.usage import UsageLimits
from pydantic_ai.capabilities import Thinking
from pydantic_ai.mcp import MCPServerStreamableHTTP
from pydantic_ai.messages import TextPart, ThinkingPart, ToolCallPart
from pydantic_ai.models import KnownModelName

# Load environment variables
load_dotenv()

# --- Configuration ---
MCP_URL = os.environ.get("MCP_URL", "http://127.0.0.1:8765/mcp")
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-3-flash-preview"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/pydantic_mcp_client-{EPOCH}.log"

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

if http_debug_logging_enabled():
    handlers = [file_handler]
    if console_logging_enabled():
        handlers.append(console_handler)
    enable_http_debug_logging(handlers=handlers)
else:
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("httpcore").setLevel(logging.WARNING)


async def run_pydantic_agent(level: str, model_name: str, delay: int, max_turns: int):
    logger.info(f"--- Pydantic AI MCP Agent Starting (Model: {model_name}) ---")

    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt",
    }
    guidance_file = guidance_map.get(level, "data/guidance_full.txt")
    guidance_cfg = load_guidance(guidance_file)
    if guidance_cfg.path:
        logger.info(f"Guidance: {guidance_cfg.path}")

    guidance_block = (
        f"\n\nGUIDANCE (follow this):\n{guidance_cfg.text}"
        if guidance_cfg.text
        else ""
    )

    reasoning_enabled = os.environ.get("AI_REASONING", "0") not in {"0", "false", "False"}
    capabilities = [Thinking()] if reasoning_enabled else []

    server = MCPServerStreamableHTTP(MCP_URL, max_retries=3)

    agent = Agent(
        model=model_name,
        toolsets=[server],
        capabilities=capabilities,
        system_prompt=(
            "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n"
            "Use the available MCP tools to play the game.\n"
            "Start with LOOK to see your surroundings.\n"
            "LOOK does not consume a game turn; do not repeat LOOK if turns did not change.\n"
            "Exits may not be listed. If unsure, try a cardinal move (NORTH/EAST/SOUTH/WEST).\n"
            "Prefer standard parser commands like LOOK, INVENTORY, N/S/E/W, TAKE <item>, USE <item>.\n"
            f"Your goal is to survive, explore, and increase your score. Stop after {max_turns} game turns or when the game ends."
            f"{guidance_block}"
        ),
    )

    prompt = (
        f"Play 'Echoes of Dustwood'. You have {max_turns} turns. "
        "Use the MCP tools to issue game commands. Start with LOOK."
    )

    provider_timer = Timer.start_new()
    try:
        async with agent.iter(prompt, usage_limits=UsageLimits(request_limit=max_turns * 4)) as agent_run:
            async for node in agent_run:
                if isinstance(node, CallToolsNode):
                    for part in node.model_response.parts:
                        if isinstance(part, ThinkingPart) and part.content.strip():
                            logger.info(f"THINKING: {part.content.strip()}")
                        elif isinstance(part, TextPart) and part.content.strip():
                            logger.info(f"AI: {part.content.strip()}")
                        elif isinstance(part, ToolCallPart):
                            args = part.args if isinstance(part.args, str) else str(part.args)
                            logger.info(f"TOOL: {part.tool_name}({args})")
    except (UnexpectedModelBehavior, UsageLimitExceeded) as e:
        logger.info(f"[GAME ENDED] {e}")
        return
    result = agent_run.result
    latency_ms = provider_timer.elapsed_ms()

    usage = result.usage()
    log_kv(
        logger,
        event="provider_call",
        client="pydantic_ai",
        model=model_name,
        latency_ms=latency_ms,
        input_tokens=getattr(usage, "input_tokens", None),
        output_tokens=getattr(usage, "output_tokens", None),
        total_tokens=getattr(usage, "total_tokens", None),
    )
    logger.info(f"\n[FINAL AGENT RESPONSE]\n{result.output}")


if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS

    asyncio.run(run_pydantic_agent(level, model, delay, max_turns))
