#!/usr/bin/env bash
set -euo pipefail

ADDR="${1:-127.0.0.1:8765}"
MCP_PATH="${2:-/mcp}"

if ! command -v curl >/dev/null 2>&1; then
  echo "curl is required"
  exit 1
fi

BASE_URL="http://${ADDR}${MCP_PATH}"
HEADERS="$(mktemp)"
BODY="$(mktemp)"

cleanup() {
  rm -f "$HEADERS" "$BODY"
}
trap cleanup EXIT

echo "=== initialize ==="
curl -s -D "$HEADERS" -o "$BODY" -X POST "$BASE_URL" \
  -H "Content-Type: application/json" \
  -H "Origin: http://localhost" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"smoke-test","version":"0.1"},"capabilities":{}}}'
cat "$BODY"
echo

SESSION_ID="$(sed -n 's/^Mcp-Session-Id: //p' "$HEADERS" | tr -d '\r')"
if [[ -z "$SESSION_ID" ]]; then
  echo "Failed to parse Mcp-Session-Id"
  exit 1
fi

echo "session=$SESSION_ID"

echo "=== tools/list ==="
curl -s -X POST "$BASE_URL" \
  -H "Content-Type: application/json" \
  -H "Origin: http://localhost" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $SESSION_ID" \
  -H "MCP-Protocol-Version: 2024-11-05" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'
echo

echo "=== tools/call ==="
curl -s -X POST "$BASE_URL" \
  -H "Content-Type: application/json" \
  -H "Origin: http://localhost" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $SESSION_ID" \
  -H "MCP-Protocol-Version: 2024-11-05" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"command","arguments":{"command":"LOOK"}}}'
echo
