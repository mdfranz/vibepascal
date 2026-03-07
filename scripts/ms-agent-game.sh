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
    echo "Echoes of Dustwood: Microsoft Agent Framework Runner"
    echo ""
    echo "Usage: ./scripts/ms-agent-game.sh [model] [goal]"
    echo ""
    echo "Arguments:"
    echo "  model         OpenAI model name (default: gpt-4o)"
    echo "  goal          The objective for the agent (default: 'Find the general store and get some water.')"
    echo ""
    echo "Examples:"
    echo "  ./scripts/ms-agent-game.sh gpt-4o-mini 'Explore the mines'"
    exit 0
fi

MODEL=${1:-gpt-4o}
GOAL=${2:-"Find the general store and get some water."}

echo "--- Starting MS Agent (Model: $MODEL, Goal: $GOAL) ---"
uv run python3 scripts/ms_agent_client.py "$MODEL" "$GOAL"

echo "--- Session Complete ---"
