#!/bin/bash

# Clean up any existing state
rm -f save.ini

# Display help if no arguments or --help/-h requested
if [[ $# -eq 0 || "$1" == "--help" || "$1" == "-h" ]]; then
    echo "Echoes of Dustwood: AI Player Runner"
    echo ""
    echo "Usage: ./ai-game.sh [difficulty] [model] [delay]"
    echo ""
    echo "Arguments:"
    echo "  difficulty    full, medium, or minimal (default: full)"
    echo "  model         pydantic-ai model string (default: google-gla:gemini-1.5-flash)"
    echo "  delay         Seconds to wait between turns (default: 5)"
    echo ""
    echo "Examples:"
    echo "  ./ai-game.sh full                        # Run with full help and Gemini"
    echo "  ./ai-game.sh medium openai:gpt-4o 10     # OpenAI with 10s delay"
    echo "  ./ai-game.sh minimal ollama:llama3 2     # Local Llama with 2s delay"
    exit 0
fi

LEVEL=${1:-full}
MODEL=${2:-google-gla:gemini-1.5-flash}
DELAY=${3:-5}

echo "--- Starting Echoes of Dustwood Sidecar ---"
# Start the sidecar in the background
uv run --with fastapi --with uvicorn python3 sidecar.py > sidecar.log 2>&1 &
SIDECAR_PID=$!

# Ensure sidecar is killed on exit
trap "kill $SIDECAR_PID" EXIT

echo "Waiting for sidecar to be ready..."
sleep 3

echo "--- Starting AI Player (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s) ---"
uv run python3 ai_client.py "$LEVEL" "$MODEL" "$DELAY"

echo "--- Game Complete ---"
