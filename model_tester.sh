#!/bin/bash
set -euo pipefail

# Echoes of Dustwood: Model Comparison Tester
# This script runs both AI backends sequentially for comparison.

MODEL=${1:-google-gla:gemini-3-flash-preview}
LEVEL=${2:-minimal}
TURNS=${3:-25}
DELAY=${4:-1}
SEED=${5:-42}

# Configuration
export GAME_SEED=$SEED
export AI_REASONING=1

echo "===================================================="
echo "🤖 DUSTWOOD MODEL TESTER"
echo "===================================================="
echo "Model:      $MODEL"
echo "Difficulty: $LEVEL"
echo "Turns:      $TURNS"
echo "Seed:       $SEED"
echo "===================================================="

# Ensure binary is built
make build > /dev/null 2>&1

echo ""
echo "🚀 [1/2] Launching Pydantic AI Client..."
echo "----------------------------------------------------"
uv run python3 scripts/ai_client.py "$LEVEL" "$MODEL" "$DELAY" "$TURNS"

echo ""
echo "🚀 [2/2] Launching Strands SDK Client..."
echo "----------------------------------------------------"
# strands_ai_client.py includes a mapper to handle Pydantic-style model names
uv run python3 scripts/strands_ai_client.py "$LEVEL" "$MODEL" "$DELAY" "$TURNS"

echo ""
echo "===================================================="
echo "📊 COMPARISON RESULTS"
echo "===================================================="
echo -n "Pydantic AI Score: "
ls -t logs/ai_client-*.log | head -n 1 | xargs grep "🏆 \[FINAL SCORE\]" | awk '{print $NF}' || echo "N/A"
echo -n "Strands SDK Score:  "
ls -t logs/strands_ai_client-*.log | head -n 1 | xargs grep "🏆 \[FINAL SCORE\]" | awk '{print $NF}' || echo "N/A"
echo "===================================================="

echo ""
echo "✅ TEST COMPLETE"
echo "===================================================="
echo "Detailed logs are available in the logs/ directory:"
echo " - Pydantic: logs/ai_client-*.log"
echo " - Strands:  logs/strands_ai_client-*.log"
echo "===================================================="
