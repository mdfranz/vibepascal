#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR"

# Ensure directories exist
mkdir -p logs data

# Ensure the game binary is up to date
if ! make build > /dev/null 2>&1; then
    echo "Failed to compile. Please install Free Pascal (fpc)."
    exit 1
fi

# Display help if requested
usage() {
    echo "Echoes of Dustwood: Agno MCP Runner"
    echo ""
    echo "Note: This script requires the Go MCP server to be running."
    echo "      You can start it with: ./bin/dustwood --mcp"
    echo ""
    echo "Usage: ./agno-mcp-game.sh <model> <max_turns> [delay] [difficulty] [--summarize] [--windowing] [--window-size SIZE] [--session-id ID]"
    echo ""
    echo "Arguments:"
    echo "  model             Model name (e.g. google:gemini-3.5-flash, anthropic:claude-3-5-sonnet-20241022) (required)"
    echo "  max_turns         Maximum turns before stopping (required)"
    echo "  delay             Seconds to wait between turns (default: 1)"
    echo "  difficulty        full, medium, minimal (default: full)"
    echo "  --summarize       Enable memory manager for long context compression"
    echo "  --windowing, -w   Enable sliding window history (disabled by default)"
    echo "  --window-size, -n Window size in game turns (default: 6)"
    echo "  --session-id ID   Resume or create a named session"
    echo ""
    echo "Examples:"
    echo "  ./agno-mcp-game.sh google:gemini-3.5-flash 25 1 full"
    echo "  ./agno-mcp-game.sh anthropic:claude-3-5-sonnet-20241022 25 1 full --summarize"
    echo "  ./agno-mcp-game.sh google:gemini-3.5-flash 25 1 full --session-id mysave"
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

echo "--- Starting Agno MCP Agent (Level: $LEVEL, Model: $MODEL, Delay: ${DELAY}s, Max Turns: $MAX_TURNS) ---"
echo "--- Ensure MCP Server is running at http://127.0.0.1:8765/mcp ---"
uv run --project packages/agno python3 packages/agno/agno_mcp_client.py "$LEVEL" "$MODEL" "$DELAY" "$MAX_TURNS" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

echo "--- Session Complete ---"
