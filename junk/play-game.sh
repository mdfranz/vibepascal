#!/bin/bash
set -euo pipefail

# Echoes of Dustwood: Multi-Client Standard Runner
# This script runs the standard AI clients sequentially for a given model.
# ADK is MCP-only, so this script auto-starts a local MCP server when needed.

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    echo "Usage: ./play-game.sh [model] [difficulty] [delay] [max_turns]"
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
MCP_ADDR=${MCP_ADDR:-"127.0.0.1:8765"}
MCP_PATH=${MCP_PATH:-"/mcp"}
MCP_URL="http://${MCP_ADDR}${MCP_PATH}"
MCP_LOG="logs/mcp-server-play-game.log"
MCP_SERVER_PID=""
# Standard clients need a headless game binary; default to Go build in this repo.
export DUSTWOOD_BIN=${DUSTWOOD_BIN:-"bin/dustwood-go"}

cleanup() {
    if [[ -n "${MCP_SERVER_PID:-}" ]]; then
        kill "${MCP_SERVER_PID}" >/dev/null 2>&1 || true
        wait "${MCP_SERVER_PID}" >/dev/null 2>&1 || true
    fi
}
trap cleanup EXIT

ensure_mcp_server() {
    if curl -sS --max-time 2 -o /dev/null "$MCP_URL"; then
        echo "--- MCP server already reachable at ${MCP_URL} ---"
        return 0
    fi

    echo "--- Starting local MCP server for ADK at ${MCP_URL} ---"
    ./bin/dustwood-go --mcp-http --mcp-addr "$MCP_ADDR" --mcp-json-response >"$MCP_LOG" 2>&1 &
    MCP_SERVER_PID=$!

    for _ in {1..20}; do
        if curl -sS --max-time 2 -o /dev/null "$MCP_URL"; then
            echo "--- MCP server started (pid: ${MCP_SERVER_PID}) ---"
            return 0
        fi
        if ! kill -0 "$MCP_SERVER_PID" >/dev/null 2>&1; then
            break
        fi
        sleep 0.2
    done

    echo "Failed to reach MCP server at ${MCP_URL}. Check ${MCP_LOG}."
    return 1
}

is_gemini_model() {
    local model_lower="${1,,}"
    [[ "$model_lower" == google-gla:gemini-* ]] \
        || [[ "$model_lower" == google:gemini-* ]] \
        || [[ "$model_lower" == gemini/* ]] \
        || [[ "$model_lower" == gemini-* ]]
}

echo "================================================================"
echo "STARTING MULTI-CLIENT GAME SESSION"
echo "Model: $MODEL, Level: $LEVEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS"
echo "================================================================"

# Map model for different frameworks if necessary
# Pydantic AI (ai-game.sh) handles google-gla: prefixes
# LiteLLM (strands-ai-game.sh) handles gemini/ prefixes

STRANDS_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    STRANDS_MODEL="gemini/${MODEL#google-gla:}"
fi

PYDANTIC_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    PYDANTIC_MODEL="google:${MODEL#google-gla:}"
fi

AGNO_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    AGNO_MODEL="gemini/${MODEL#google-gla:}"
fi

MS_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    MS_MODEL="${MODEL#google-gla:}"
fi

ADK_MODEL="$MODEL"
if [[ "$MODEL" == google-gla:* ]]; then
    ADK_MODEL="${MODEL#google-gla:}"
elif [[ "$MODEL" == google:* ]]; then
    ADK_MODEL="${MODEL#google:}"
fi

ensure_mcp_server

echo ""
echo "--- Running Client 1: Pydantic AI (Standard) ---"
./pydantic-game.sh "$LEVEL" "$PYDANTIC_MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "--- Running Client 2: Agno (Standard) ---"
./agno-game.sh "$LEVEL" "$AGNO_MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "--- Running Client 3: Microsoft Agent Framework (Standard) ---"
if is_gemini_model "$MODEL"; then
    echo "--- Skipping MS Agent for Gemini model (${MS_MODEL}): unsupported thought_signature flow ---"
else
    ./ms-agent-game.sh "$LEVEL" "$MS_MODEL" "$DELAY" "$MAX_TURNS"
fi

echo ""
echo "--- Running Client 4: Strands AI (Standard) ---"
./strands-game.sh "$LEVEL" "$STRANDS_MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "--- Running Client 5: ADK (MCP) ---"
./adk-mcp-game.sh "$LEVEL" "$ADK_MODEL" "$DELAY" "$MAX_TURNS"

echo ""
echo "================================================================"
echo "ALL CLIENT SESSIONS COMPLETE"
echo "================================================================"
