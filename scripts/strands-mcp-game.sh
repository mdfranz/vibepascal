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
if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    echo "Echoes of Dustwood: Strands MCP Runner"
    echo ""
    echo "Note: This script requires the Go MCP server to be running."
    echo "      You can start it with: ./bin/dustwood --mcp"
    echo ""
    echo "Usage: ./scripts/strands-mcp-game.sh [difficulty] [model] [delay] [max_turns]"
    echo ""
    echo "Arguments:"
    echo "  difficulty    full, medium, minimal (default: full)"
    echo "  model         LiteLLM model name (default: gemini/gemini-3-flash-preview)"
    echo "  delay         Seconds to wait between turns (default: 1)"
    echo "  max_turns     Maximum turns before stopping (default: 25)"
    echo ""
    echo "Examples:"
    echo "  ./scripts/strands-mcp-game.sh full gemini/gemini-3-flash-preview 1 25"
    exit 0
fi

LEVEL=${1:-full}
MODEL=${2:-gemini/gemini-3-flash-preview}
DELAY=${3:-1}
MAX_TURNS=${4:-25}

echo "--- Starting Strands MCP Agent (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS) ---"
echo "--- Ensure MCP Server is running at http://127.0.0.1:8765/mcp ---"
uv run python3 scripts/strands_mcp_client.py "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS"

echo "--- Session Complete ---"
