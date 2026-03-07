#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT_DIR"

# Ensure directories exist
mkdir -p logs data

# Ensure the game binary is up to date
if ! make build > /dev/null 2>&1; then
    echo "Failed to compile. Please install Free Pascal (fpc)."
    exit 1
fi

# Display help if requested
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    echo "Echoes of Dustwood: Agno MCP Runner"
    echo ""
    echo "Note: This script requires the Go MCP server to be running."
    echo "      You can start it with: ./bin/dustwood --mcp"
    echo ""
    echo "Usage: ./scripts/agno-mcp-game.sh [model] [goal]"
    echo ""
    echo "Arguments:"
    echo "  model         OpenAI model name (default: gpt-5-mini)"
    echo "  goal          The objective for the agent (default: 'Find the general store and get some water.')"
    echo ""
    echo "Examples:"
    echo "  ./scripts/agno-mcp-game.sh gpt-4o 'Explore the town'"
    exit 0
fi

MODEL=${1:-gpt-5-mini}
GOAL=${2:-"Find the general store and get some water."}

echo "--- Starting Agno MCP Agent (Model: $MODEL, Goal: $GOAL) ---"
echo "--- Ensure MCP Server is running at http://127.0.0.1:8765/mcp ---"
uv run python3 scripts/agno_mcp_client.py "$MODEL" "$GOAL"

echo "--- Session Complete ---"
