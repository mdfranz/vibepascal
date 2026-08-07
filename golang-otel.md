# Add OpenTelemetry to Dustwood MCP and Game

## Summary

Instrument the Go MCP server and both Go gameplay paths with vendor-neutral OpenTelemetry, exporting traces and metrics over OTLP/HTTP to Logfire or another backend. Pydantic AI will propagate its existing Logfire trace through MCP metadata, producing:

`pydantic_game_run → MCP client request → Go MCP request → game command`

Use the standard OTel Go SDK because Logfire has no dedicated Go SDK. Upgrade the project minimum to Go 1.26, as required by current OTel modules and standard toolchains. See the [Pydantic Go instrumentation guide](https://pydantic.dev/docs/logfire/instrument/go/).

## Implementation Changes

### Telemetry foundation

- Add a `Telemetry` component that initializes only when `OTEL_ENABLED=1`; otherwise it uses no-op tracing and metrics.
- Configure OTLP/HTTP trace and metric exporters from `OTEL_EXPORTER_OTLP_*`.
- Use `OTEL_SERVICE_NAME`, defaulting to `dustwood-go`, plus standard resource attributes.
- Set global W3C `traceparent`/`tracestate` propagator via `otel.SetTextMapPropagator(propagation.NewCompositeTextMapPropagator(propagation.TraceContext{}, propagation.Baggage{}))` and an OTel asynchronous error handler that writes to stderr.
- Continue without telemetry if initialization or export fails.
- Flush both providers with a five-second shutdown timeout.
- Keep the existing `slog` output local; do not bridge logs to OTLP in this iteration.

### MCP distributed tracing

- Add one Go MCP receiving middleware through `AddReceivingMiddleware`.
- Extract lowercase `traceparent` and `tracestate` strings from request `_meta` into a `propagation.MapCarrier` and call `otel.GetTextMapPropagator().Extract(ctx, carrier)`.
- Create a server span for every MCP method and pass its context to tools, resources, and prompts.
- Use attributes `rpc.system=jsonrpc`, `rpc.jsonrpc.version=2.0`, `rpc.method`, and bounded `mcp.tool.name`.
- Record protocol and tool failures as span errors, while keeping natural game endings successful.
- Emit `mcp.server.request.count` and `mcp.server.request.duration` with bounded method, tool, and outcome dimensions.
- Never record raw arguments, commands, results, narrative output, authorization data, or arbitrary metadata in Go-side telemetry.

On the Pydantic client, call `logfire.instrument_mcp()` once after `logfire.configure()` whenever `LOGFIRE_ENABLED` is enabled. Preserve the existing Pydantic AI instrumentation and payload behavior. This creates the client-side MCP span and injects trace context into MCP metadata, following [Logfire's distributed MCP instrumentation model](https://pydantic.dev/docs/logfire/integrations/llms/mcp/).

### Game instrumentation

- Share instrumentation between MCP and interactive CLI command execution.
- Wrap commands in `dustwood.game.command <canonical-verb>` spans nested beneath the MCP span or CLI session span.
- Normalize verbs to a fixed vocabulary; map unrecognized input to `unknown` and omit nouns and raw input.
- Record safe before/after state: execution mode, turn, room ID, score, thirst, horse thirst, and whether play ended.
- Emit `dustwood.game.command.count`, `dustwood.game.command.duration`, and latest-value gauges for turn, score, thirst, and horse thirst.
- Trace game initialization and reset separately without recording the seed value.
- Preserve all existing gameplay, turn-limit, output, and MCP `IsError` semantics.

### Lifecycle and interfaces

- Inject `*Telemetry` into `MCPServer`.
- Pass `context.Context` into MCP/CLI command execution and game initialization.
- Make `RunMCPHTTP` and `RunMCPStdio` context-aware.
- Wrap the interactive CLI loop in a game-session span.
- Replace fatal exits inside the run path with returned errors so deferred telemetry shutdown executes.
- Gracefully stop the HTTP server on SIGINT or SIGTERM before flushing telemetry.
- Upgrade `go.mod` to Go 1.26 (`go 1.26.0`) and add the OTel SDK plus OTLP HTTP trace and metric exporters.

## Configuration and Documentation

Document the following Logfire setup in the README and observability guide:

```bash
export OTEL_ENABLED=1
export OTEL_EXPORTER_OTLP_ENDPOINT=https://logfire-us.pydantic.dev
export OTEL_EXPORTER_OTLP_PROTOCOL=http/protobuf
export OTEL_EXPORTER_OTLP_HEADERS='Authorization=<write-token>'
export OTEL_SERVICE_NAME=dustwood-go
```

Also document:

- Explicitly set `OTEL_EXPORTER_OTLP_PROTOCOL=http/protobuf` to ensure HTTP Protobuf export format required by Logfire.
- Use `https://logfire-eu.pydantic.dev` for an EU-region project.
- Standard OTel environment variables may override exporter and resource behavior.
- The Go process requires a project write token and does not consume Python's cached `logfire auth` credentials.
- Telemetry is disabled by default unless `OTEL_ENABLED=1`.

## Test Plan

### MCP tracing

- Use an in-memory span exporter to verify that a known `_meta.traceparent` becomes the Go span's remote parent.
- Verify that missing or malformed context safely starts or continues a local trace.
- Verify tool names and errors are recorded while payloads and arbitrary metadata are absent.
- Verify the handler context contains the created span.

### Metrics and gameplay

- Use an in-memory/manual metric reader to verify MCP count and duration instruments use bounded attributes.
- Verify commands update count, duration, score, turn, and thirst instruments exactly once.
- Verify reset initializes gauges without exposing the seed.
- Test gameplay tracing for MCP and CLI sources, including invalid commands, reset, timeout/death, and normal commands.
- Confirm terminal game states do not set error status or change returned output or state.

### Failure and lifecycle behavior

- Confirm an absent or false `OTEL_ENABLED` causes no exporter activity.
- Simulate initialization and export failures and confirm they produce warnings without interrupting gameplay or MCP.
- Confirm normal CLI exit and HTTP shutdown flush providers within five seconds.
- Run `go test ./...`, `go vet ./...`, and the existing MCP smoke test.
- With Logfire credentials, perform a one-turn Pydantic run and verify the cross-language trace hierarchy and incoming game/MCP metrics in Live View.

## Assumptions

- Initial distributed correlation targets Pydantic AI only. Agno, Strands, and ADK requests still create valid standalone Go server traces.
- OTLP remains backend-neutral, although documentation uses Logfire as the primary example.
- Go-side telemetry records safe metadata only. Existing Python Logfire payload capture remains unchanged.
- Pascal instrumentation, OTLP log export, dashboards, alerts, and instrumentation of the other three clients are outside this iteration.
