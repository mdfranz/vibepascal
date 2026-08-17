#!/bin/bash
set -euo pipefail

export AI_REASONING=1
export LOG_CONSOLE=1

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR"

# --- OpenTelemetry & Logfire Observability ------------------------------
export OTEL_ENABLED="${OTEL_ENABLED:-1}"
export OTEL_SERVICE_NAME="${OTEL_SERVICE_NAME:-dustwood-go}"
export OTEL_EXPORTER_OTLP_PROTOCOL="${OTEL_EXPORTER_OTLP_PROTOCOL:-http/protobuf}"

# Read the repository-local project credential unless the caller supplied a token.
LOGFIRE_CREDS="${LOGFIRE_CREDS:-$ROOT_DIR/.logfire/logfire_credentials.json}"
if [[ -z "${LOGFIRE_TOKEN:-}" && -f "$LOGFIRE_CREDS" ]]; then
    read -r DETECTED_ENDPOINT LOGFIRE_TOKEN < <(
        python3 - "$LOGFIRE_CREDS" <<'PY'
import json
import sys

with open(sys.argv[1]) as f:
    credentials = json.load(f)

print(credentials.get("logfire_api_url", ""), credentials.get("token", ""))
PY
    )
    export LOGFIRE_TOKEN
    if [[ -z "${OTEL_EXPORTER_OTLP_ENDPOINT:-}" && -n "$DETECTED_ENDPOINT" ]]; then
        export OTEL_EXPORTER_OTLP_ENDPOINT="$DETECTED_ENDPOINT"
    fi
fi

if [[ -n "${LOGFIRE_TOKEN:-}" && -z "${OTEL_EXPORTER_OTLP_HEADERS:-}" ]]; then
    export OTEL_EXPORTER_OTLP_HEADERS="Authorization=${LOGFIRE_TOKEN}"
fi

export OTEL_EXPORTER_OTLP_ENDPOINT="${OTEL_EXPORTER_OTLP_ENDPOINT:-https://logfire-us.pydantic.dev}"

# Echoes of Dustwood: Multi-Client MCP Runner
# This script runs MCP AI clients sequentially for a given model.

MCP_ADDR="127.0.0.1:8765"
MCP_URL="http://${MCP_ADDR}/mcp"
SERVER_PID=""

usage() {
    echo "Usage: ./play-mcp-game.sh <model> <max_turns> [delay] [difficulty] [--summarize] [--windowing] [--window-size SIZE] [--session-id ID] [--allow-restart]"
    echo ""
    echo "Manages its own Go MCP server (bin/dustwood-go) - one fresh instance per client,"
    echo "restarted between each of the 4 framework runs below, rather than requiring one to"
    echo "already be running. This matters because the server's own --turns limit (matched to"
    echo "max_turns here) and --allow-restart flag are per-process: since --allow-restart"
    echo "defaults OFF (see src/golang/mcp_server.go), a model can no longer reset_game/retry"
    echo "after GAME OVER, so each of the 4 clients needs its own clean server instance rather"
    echo "than sharing one long-lived process across all 4 runs."
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
    echo "  --allow-restart   Pass --allow-restart to the MCP server: let a model reset_game/retry"
    echo "                    after GAME OVER instead of one attempt per client (old behavior)"
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
ALLOW_RESTART=0
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
        --allow-restart)
            ALLOW_RESTART=1
            shift
            ;;
        *)
            echo "Unknown argument: $1"
            usage
            ;;
    esac
done

# --- MCP server lifecycle -----------------------------------------------
# One fresh server per client run (not one shared long-lived process): the
# server's --allow-restart is off by default (src/golang/mcp_server.go), so a
# model gets exactly one attempt per process. Reusing one process across all
# 4 clients would let only the first client's game actually start.

stop_mcp_server() {
    if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    SERVER_PID=""
}

trap stop_mcp_server EXIT INT TERM

start_mcp_server() {
    stop_mcp_server

    local server_args=(--mcp-http --mcp-addr "$MCP_ADDR" --mcp-json-response --turns "$MAX_TURNS")
    if [[ "$ALLOW_RESTART" -eq 1 ]]; then
        server_args+=(--allow-restart)
    fi

    ./bin/dustwood-go "${server_args[@]}" &
    SERVER_PID=$!

    local waited=0
    until curl -s -o /dev/null -w '' "$MCP_URL" -X POST \
        -H "Content-Type: application/json" -H "Accept: application/json, text/event-stream" \
        -d '{"jsonrpc":"2.0","id":0,"method":"ping"}' 2>/dev/null; do
        if ! kill -0 "$SERVER_PID" 2>/dev/null; then
            echo "MCP server exited unexpectedly during startup." >&2
            exit 1
        fi
        if [[ "$waited" -ge 10 ]]; then
            echo "MCP server did not become ready within 10s." >&2
            exit 1
        fi
        sleep 1
        waited=$((waited + 1))
    done
}

if ! make build-go > /dev/null 2>&1; then
    echo "Failed to compile the Go MCP server. Please install Go." >&2
    exit 1
fi

echo "================================================================"
echo "STARTING MULTI-CLIENT MCP SESSION"
echo "Model: $MODEL, Level: $LEVEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS"
if [[ "$ALLOW_RESTART" -eq 1 ]]; then
    echo "MCP server: --allow-restart set (models may retry after GAME OVER)"
else
    echo "MCP server: one attempt per client (default; pass --allow-restart to change)"
fi
if [[ "$OTEL_ENABLED" == "1" && -n "${OTEL_EXPORTER_OTLP_HEADERS:-}" ]]; then
    echo "MCP telemetry: OpenTelemetry enabled -> $OTEL_EXPORTER_OTLP_ENDPOINT ($OTEL_SERVICE_NAME)"
elif [[ "$OTEL_ENABLED" == "1" ]]; then
    echo "MCP telemetry: OpenTelemetry enabled (no auth token found; no-op/local fallback)"
else
    echo "MCP telemetry: OpenTelemetry disabled (OTEL_ENABLED=0)"
fi
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
start_mcp_server
./pydantic-mcp-game.sh "$MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "--- Running Client 2: Agno (MCP) ---"
start_mcp_server
./agno-mcp-game.sh "$AGNO_MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "--- Running Client 3: Strands AI (MCP) ---"
start_mcp_server
./strands-mcp-game.sh "$STRANDS_MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo ""
echo "--- Running Client 4: ADK (MCP) ---"
start_mcp_server
./adk-mcp-game.sh "$ADK_MODEL" "$MAX_TURNS" "$DELAY" "$LEVEL" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

stop_mcp_server

echo ""
echo "================================================================"
echo "ALL MCP CLIENT SESSIONS COMPLETE"
echo "================================================================"
