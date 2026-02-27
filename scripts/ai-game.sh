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
    echo "  difficulty    full, medium, minimal, or win (default: full)"
    echo "  model         pydantic-ai model string (default: google-gla:gemini-1.5-flash)"
    echo "  delay         Seconds to wait between turns (default: 1)"
    echo "  max_turns     Maximum turns before stopping (default: 50)"
    echo ""
    echo "Examples:"
    echo "  ./scripts/ai-game.sh full                        # Run with full help and Gemini"
    echo "  ./scripts/ai-game.sh medium openai:gpt-4o 10     # OpenAI with 10s delay"
    echo "  ./scripts/ai-game.sh medium anthropic:claude-sonnet-4-6 5"
    echo "  ./scripts/ai-game.sh minimal ollama:llama3 2     # Local Llama with 2s delay"
    echo "  ./scripts/ai-game.sh win                         # Scripted win path"
    exit 0
fi

LEVEL=${1:-full}
MODEL=${2:-google-gla:gemini-1.5-flash}
DELAY=${3:-1}
MAX_TURNS=${4:-50}

echo "--- Starting Echoes of Dustwood Sidecar ---"
# Start the sidecar in the background
uv run --with fastapi --with uvicorn python3 scripts/sidecar.py > logs/sidecar.log 2>&1 &
SIDECAR_PID=$!

# Ensure sidecar is killed on exit
trap "kill $SIDECAR_PID" EXIT

echo "Waiting for sidecar to be ready..."
sleep 3

if [[ "$LEVEL" == "win" ]]; then
    echo "--- Starting AI Player (Level: $LEVEL, Model: scripted, Delay: ${DELAY}s) ---"
else
    echo "--- Starting AI Player (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s) ---"
fi
uv run python3 scripts/ai_client.py "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS"

echo "--- Game Complete ---"
