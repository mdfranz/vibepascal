#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR"

# Ensure directories exist
mkdir -p logs data

# Send this run's traces to Logfire by default (opt out with LOGFIRE_ENABLED=0).
# Requires `logfire auth` (or LOGFIRE_TOKEN) to have been run once; see
# packages/shared/OBSERVABILITY.md.
export LOGFIRE_ENABLED="${LOGFIRE_ENABLED:-1}"

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

# Ensure the game binary is up to date
if ! make build > /dev/null 2>&1; then
    echo "Failed to compile. Please install Free Pascal (fpc)."
    exit 1
fi

# Display help if requested
usage() {
    echo "Echoes of Dustwood: Pydantic AI MCP Runner"
    echo ""
    echo "Note: This script requires the Go MCP server to be running, e.g.:"
    echo "      ./bin/dustwood-go --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response --turns \$MAX_TURNS"
    echo ""
    echo "      Match the server's --turns to this script's max_turns - they're independent, and"
    echo "      the server's turn limit governs the in-game day/night cutoff. The server's"
    echo "      --allow-restart flag (default off) also means one server process = one attempt:"
    echo "      restart it between separate invocations of this script, or the next run's own"
    echo "      required bootstrap reset will be rejected too. See CLAUDE.md."
    echo ""
    echo "Usage: ./pydantic-mcp-game.sh <model> <max_turns> [delay] [difficulty] [--summarize] [--windowing] [--window-size SIZE] [--session-id ID]"
    echo ""
    echo "Arguments:"
    echo "  model             Model name (e.g. google:gemini-3.5-flash, anthropic:claude-3-5-sonnet-20241022) (required)"
    echo "  max_turns         Maximum turns before stopping (required)"
    echo "  delay             Seconds to wait between turns (default: 1)"
    echo "  difficulty        full, medium, minimal (default: full)"
    echo "  --summarize       Enable history summarization to manage long context"
    echo "  --windowing, -w   Enable sliding window history (disabled by default)"
    echo "  --window-size, -n Window size in game turns (default: 6)"
    echo "  --session-id ID   Resume or create a named session"
    echo ""
    echo "Env vars:"
    echo "  LOGFIRE_ENABLED    Send traces to Logfire (default: 1). Set to 0 to disable."
    echo "  LOGFIRE_TOKEN      Logfire project write token (falls back to .logfire/ credentials)"
    echo "  LOGFIRE_ENVIRONMENT  Logfire deployment.environment tag (default: development)"
    echo ""
    echo "Examples:"
    echo "  ./pydantic-mcp-game.sh google:gemini-3.5-flash 25 1 full"
    echo "  ./pydantic-mcp-game.sh anthropic:claude-3-5-sonnet-20241022 25 1 full --summarize"
    echo "  ./pydantic-mcp-game.sh google:gemini-3.5-flash 25 1 full --session-id mysave"
    exit 1
}

if [[ $# -lt 2 || "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    usage
fi

MODEL=$1
MAX_TURNS=$2
DELAY=${3:-1}
LEVEL=${4:-full}

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

echo "--- Starting Pydantic AI MCP Agent (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS) ---"
echo "--- Ensure MCP Server is running at http://127.0.0.1:8765/mcp ---"
uv run --project packages/pydantic python3 packages/pydantic/pydantic_mcp_client.py "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo "--- Session Complete ---"
