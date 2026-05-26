#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR"

# Ensure directories exist
mkdir -p logs data

# Ensure the game binary is up to date
if ! make build > /dev/null 2>&1; then
    echo "Failed to compile. Please install Free Pascal (fpc)."
    exit 1
fi

# Display help if requested
usage() {
    echo "Echoes of Dustwood: ADK MCP Runner"
    echo ""
    echo "Note: This script requires the Go MCP server to be running."
    echo "      You can start it with: ./bin/dustwood-go --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response"
    echo ""
    echo "Usage: ./adk-mcp-game.sh <model> <max_turns> [delay] [difficulty]"
    echo ""
    echo "Arguments:"
    echo "  model         ADK model name (required)"
    echo "  max_turns     Maximum turns before stopping (required)"
    echo "  delay         Seconds to wait between turns (default: 1)"
    echo "  difficulty    full, medium, minimal (default: full)"
    echo ""
    echo "Examples:"
    echo "  ./adk-mcp-game.sh gemini-3.5-flash 25 1 full"
    exit 1
}

if [[ $# -lt 2 || "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    usage
fi

MODEL=$1
MAX_TURNS=$2
DELAY=${3:-1}
LEVEL=${4:-full}

echo "--- Starting ADK MCP Agent (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS) ---"
echo "--- Ensure MCP Server is running at http://127.0.0.1:8765/mcp ---"
uv run --project packages/adk python3 packages/adk/adk_mcp_client.py "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS"

echo "--- Session Complete ---"
