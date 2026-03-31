#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR"

# Models to test
MODELS=(
    "anthropic:claude-opus-4-6"
    "openai:gpt-5.2"
    "google-gla:gemini-3.1-pro-preview"
    "google-gla:gemini-3-flash-preview"
    "openai:o4-mini"
)

LEVEL=${1:-full}
TURNS=${2:-25}
DELAY=${3:-1}

echo "===================================================="
echo "🤖 PYDANTIC AI MCP MULTI-MODEL RUNNER"
echo "===================================================="
echo "Difficulty: $LEVEL"
echo "Turns:      $TURNS"
echo "Delay:      ${DELAY}s"
echo "===================================================="

# Ensure binary is built
make build > /dev/null 2>&1

for MODEL in "${MODELS[@]}"; do
    echo ""
    echo "🚀 Launching Model: $MODEL"
    echo "----------------------------------------------------"
    
    # Enable reasoning for Gemini 3.1 Pro specifically if needed, 
    # or just enable it for all to see how they handle it.
    if [[ "$MODEL" == *"gemini-3.1-pro"* ]]; then
        export AI_REASONING=1
    else
        export AI_REASONING=0
    fi
    
    uv run python3 scripts/pydantic_mcp_client.py "$LEVEL" "$MODEL" "$DELAY" "$TURNS"
    
    echo "----------------------------------------------------"
    echo "✅ Finished $MODEL"
done

echo ""
echo "===================================================="
echo "📊 SUMMARY OF RUNS"
echo "===================================================="
for MODEL in "${MODELS[@]}"; do
    # Find the latest log for this model
    LATEST_LOG=$(ls -t logs/pydantic_mcp_client-*.log | xargs grep -l "Model: $MODEL" | head -n 1 || true)
    
    if [ -n "$LATEST_LOG" ]; then
        SCORE=$(grep "Final Score:" "$LATEST_LOG" | sed 's/.*Final Score: //; s/ points//' || echo "N/A")
        if [ "$SCORE" = "N/A" ]; then
            # Try finding score in final agent response if not formatted
            SCORE=$(tail -n 10 "$LATEST_LOG" | grep -oE "score was [0-9]+" | grep -oE "[0-9]+" || echo "N/A")
        fi
        echo "Model: $MODEL -> Score: $SCORE"
    else
        echo "Model: $MODEL -> No log found"
    fi
done
echo "===================================================="
