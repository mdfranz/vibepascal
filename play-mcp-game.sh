#!/bin/bash
set -euo pipefail

export AI_REASONING=1
export LOG_CONSOLE=1

# Echoes of Dustwood: Multi-Client MCP Runner
# This script runs MCP AI clients sequentially for a given model.

usage() {
    echo "Usage: ./play-mcp-game.sh <model> <max_turns> [delay] [difficulty] [--summarize] [--windowing] [--window-size SIZE] [--session-id ID]"
    echo ""
    echo "Note: This script requires the Go MCP server to be running."
    echo "      ./bin/dustwood-go --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response"
    echo ""
    echo "Arguments:"
    echo "  model             Model name (required)"
    echo "  max_turns         Max turns per session (required)"
    echo "  delay             Seconds between turns (default: 1)"
    echo "  difficulty        full, medium, minimal (default: full)"
    echo "  --summarize       Enable summarization on all clients"
    echo "  --windowing, -w   Enable sliding window history (disabled by default)"
    echo "  --window-size, -n Window size in game turns (default: 6)"
    echo "  --session-id ID   Resume or create a named session"
    echo ""
    echo "Examples:"
    echo "  ./play-mcp-game.sh google-gla:gemini-3-flash-preview 15"
    echo "  ./play-mcp-game.sh google-gla:gemini-3-flash-preview 25 1 full --windowing"
    exit 1
}

if [[ $# -lt 2 || "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    usage
fi

MODEL=$1
MAX_TURNS=$2
DELAY=${3:-"1"}
LEVEL=${4:-"full"}

EXTRA_ARGS=()
shift 4 2>/dev/null || shift $# 2>/dev/null || true
while [[ $# -gt 0 ]]; do
    case "$1" in
        --summarize|-s)
            EXTRA_ARGS+=(--summarize)
            shift
            ;;
        --windowing|-w)
            EXTRA_ARGS+=(--windowing)
            shift
            ;;
        --window-size|-n)
            EXTRA_ARGS+=(--window-size "$2")
            shift 2
            ;;
        --session-id)
            EXTRA_ARGS+=(--session-id "$2")
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            usage
            ;;
    esac
done

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
elif [[ "$MODEL" == google:* ]]; then
    STRANDS_MODEL="gemini/${MODEL#google:}"
elif [[ "$MODEL" == anthropic:* ]]; then
    STRANDS_MODEL="anthropic/${MODEL#anthropic:}"
fi

AGNO_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    AGNO_MODEL="gemini/${MODEL#google-gla:}"
elif [[ "$MODEL" == google:* ]]; then
    AGNO_MODEL="gemini/${MODEL#google:}"
elif [[ "$MODEL" == anthropic:* ]]; then
    AGNO_MODEL="${MODEL#anthropic:}"
fi

ADK_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    ADK_MODEL="${MODEL#google-gla:}"
elif [[ "$MODEL" == google:* ]]; then
    ADK_MODEL="${MODEL#google:}"
elif [[ "$MODEL" == gemini/* ]]; then
    ADK_MODEL="${MODEL#gemini/}"
fi

echo ""
echo "--- Running Client 1: Pydantic AI (MCP) ---"
./pydantic-mcp-game.sh "$MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "--- Running Client 2: Agno (MCP) ---"
./agno-mcp-game.sh "$AGNO_MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "--- Running Client 3: Strands AI (MCP) ---"
./strands-mcp-game.sh "$STRANDS_MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "--- Running Client 4: ADK (MCP) ---"
./adk-mcp-game.sh "$ADK_MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "--- Running Client 5: Microsoft Agent Framework (.NET) (MCP) ---"
./maf-mcp-game.sh "$MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "================================================================"
echo "ALL MCP CLIENT SESSIONS COMPLETE"
echo "================================================================"

