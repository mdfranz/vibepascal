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
if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    echo "Echoes of Dustwood: Agno Agent Runner"
    echo ""
    echo "Usage: ./agno-game.sh [difficulty] [model] [delay] [max_turns]"
    echo ""
    echo "Arguments:"
    echo "  difficulty    full, medium, minimal (default: full)"
    echo "  model         OpenAI model name (default: gpt-5-mini)"
    echo "  delay         Seconds to wait between turns (default: 1)"
    echo "  max_turns     Maximum turns before stopping (default: 25)"
    echo ""
    echo "Examples:"
    echo "  ./agno-game.sh full gpt-4o-mini 1 25"
    exit 0
fi

LEVEL=${1:-full}
MODEL=${2:-gpt-5-mini}
DELAY=${3:-1}
MAX_TURNS=${4:-25}

echo "--- Starting Agno Agent (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS) ---"
uv run python3 scripts/agno_client.py "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS"

echo "--- Session Complete ---"
