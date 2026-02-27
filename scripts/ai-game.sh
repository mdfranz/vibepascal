#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT_DIR"

# Ensure directories exist
mkdir -p logs data

# Clean up any existing state
rm -f data/save.ini

# Ensure the game binary is up to date
if ! make build > /dev/null 2>&1; then
    echo "Failed to compile. Please install Free Pascal (fpc)."
    exit 1
fi

# Display help if no arguments or --help/-h requested
if [[ $# -eq 0 || "$1" == "--help" || "$1" == "-h" ]]; then
    echo "Echoes of Dustwood: AI Player Runner"
    echo ""
    echo "Usage: ./scripts/ai-game.sh [difficulty] [model] [delay] [max_turns]"
    echo ""
    echo "Arguments:"
    echo "  difficulty    full, medium, minimal (default: full)"
    echo "  model         pydantic-ai model string (default: google-gla:gemini-3-flash-preview)"
    echo "  delay         Seconds to wait between turns (default: 1)"
    echo "  max_turns     Maximum turns before stopping (default: 50)"
    echo ""
    echo "Examples:"
    echo "  ./scripts/ai-game.sh full                        # Run with full help and Gemini"
    echo "  ./scripts/ai-game.sh medium openai:gpt-4o 2      # OpenAI with 2s delay"
    echo "  ./scripts/ai-game.sh minimal ollama:llama3 1     # Local Llama"
    exit 0
fi

LEVEL=${1:-full}
MODEL=${2:-google-gla:gemini-3-flash-preview}
DELAY=${3:-1}
MAX_TURNS=${4:-50}

echo "--- Starting AI Player (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s) ---"
# Run the AI client directly (it now manages the game process itself)
uv run python3 scripts/ai_client.py "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS"

echo "--- Session Complete ---"
