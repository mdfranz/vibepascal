#!/bin/bash
set -euo pipefail

# Echoes of Dustwood: Multi-Client MCP Runner
# This script runs the 4 MCP AI clients sequentially for a given model.

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    echo "Usage: ./play-mcp-game.sh [model] [difficulty] [delay] [max_turns]"
    echo ""
    echo "Note: This script requires the Go MCP server to be running."
    echo "      ./bin/dustwood-go --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response"
    echo ""
    echo "Arguments:"
    echo "  model         Model name (default: google-gla:gemini-3-flash-preview)"
    echo "  difficulty    full, medium, minimal (default: full)"
    echo "  delay         Seconds between turns (default: 1)"
    echo "  max_turns     Max turns per session (default: 25)"
    exit 0
fi

MODEL=${1:-"google-gla:gemini-3-flash-preview"}
LEVEL=${2:-"full"}
DELAY=${3:-"1"}
MAX_TURNS=${4:-"25"}

echo "================================================================"
echo "STARTING MULTI-CLIENT MCP SESSION"
echo "Model: $MODEL, Level: $LEVEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS"
echo "================================================================"

# Map model for different frameworks if necessary
# Pydantic AI (pydantic-mcp-game.sh) handles google-gla: prefixes
# LiteLLM (strands-mcp-game.sh) handles gemini/ prefixes

STRANDS_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    STRANDS_MODEL="gemini/${MODEL#google-gla:}"
fi

AGNO_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    AGNO_MODEL="gemini/${MODEL#google-gla:}"
fi

MS_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    MS_MODEL="${MODEL#google-gla:}"
fi

echo ""
echo "--- Running Client 1: Pydantic AI (MCP) ---"
./scripts/pydantic-mcp-game.sh "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "--- Running Client 2: Agno (MCP) ---"
./scripts/agno-mcp-game.sh "$LEVEL" "$AGNO_MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "--- Running Client 3: Microsoft Agent Framework (MCP) ---"
./scripts/ms-mcp-agent-game.sh "$LEVEL" "$MS_MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "--- Running Client 4: Strands AI (MCP) ---"
./scripts/strands-mcp-game.sh "$LEVEL" "$STRANDS_MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "================================================================"
echo "ALL MCP CLIENT SESSIONS COMPLETE"
echo "================================================================"
