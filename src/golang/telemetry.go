package main

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"strings"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"go.opentelemetry.io/otel/codes"
	"go.opentelemetry.io/otel/exporters/otlp/otlpmetric/otlpmetrichttp"
	"go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracehttp"
	"go.opentelemetry.io/otel/metric"
	"go.opentelemetry.io/otel/metric/noop"
	"go.opentelemetry.io/otel/propagation"
	sdkmetric "go.opentelemetry.io/otel/sdk/metric"
	"go.opentelemetry.io/otel/sdk/resource"
	sdktrace "go.opentelemetry.io/otel/sdk/trace"
	semconv "go.opentelemetry.io/otel/semconv/v1.43.0"
	"go.opentelemetry.io/otel/trace"
	nooptrace "go.opentelemetry.io/otel/trace/noop"
)

// Telemetry manages OpenTelemetry tracing, metrics, and MCP middleware.
type Telemetry struct {
	Enabled        bool
	TracerProvider *sdktrace.TracerProvider
	MeterProvider  *sdkmetric.MeterProvider
	Tracer         trace.Tracer
	Meter          metric.Meter

	// Metrics instruments
	mcpServerRequestCount    metric.Int64Counter
	mcpServerRequestDuration metric.Float64Histogram
	gameCommandCount         metric.Int64Counter
	gameCommandDuration      metric.Float64Histogram
	turnGauge                metric.Int64Gauge
	scoreGauge               metric.Int64Gauge
	thirstGauge              metric.Int64Gauge
	horseThirstGauge         metric.Int64Gauge
}

// isOtelEnabled checks environment flags to determine whether OpenTelemetry should be activated.
func isOtelEnabled() bool {
	val := strings.ToLower(strings.TrimSpace(os.Getenv("OTEL_ENABLED")))
	return val == "1" || val == "true" || val == "yes" || val == "on"
}

// InitTelemetry initializes OpenTelemetry tracing and metrics if OTEL_ENABLED is set.
// If telemetry is disabled or initialization fails, a no-op Telemetry instance is returned safely.
func InitTelemetry(ctx context.Context) (*Telemetry, error) {
	if !isOtelEnabled() {
		return NewNoopTelemetry(), nil
	}

	serviceName := os.Getenv("OTEL_SERVICE_NAME")
	if serviceName == "" {
		serviceName = "dustwood-go"
	}

	// 1. Composite TextMapPropagator for distributed context propagation
	propagator := propagation.NewCompositeTextMapPropagator(
		propagation.TraceContext{},
		propagation.Baggage{},
	)
	otel.SetTextMapPropagator(propagator)

	// 2. Asynchronous error handler logging to slog
	otel.SetErrorHandler(otel.ErrorHandlerFunc(func(err error) {
		slog.Warn("otel error", "err", err)
	}))

	// 3. Resource specification
	res, err := resource.Merge(
		resource.Default(),
		resource.NewWithAttributes(
			semconv.SchemaURL,
			semconv.ServiceNameKey.String(serviceName),
		),
	)
	if err != nil {
		slog.Warn("failed to create otel resource; falling back to no-op telemetry", "err", err)
		return NewNoopTelemetry(), nil
	}

	// 4. Trace exporter and TracerProvider
	traceExporter, err := otlptracehttp.New(ctx)
	if err != nil {
		slog.Warn("failed to initialize OTLP trace exporter; falling back to no-op telemetry", "err", err)
		return NewNoopTelemetry(), nil
	}

	tp := sdktrace.NewTracerProvider(
		sdktrace.WithBatcher(traceExporter),
		sdktrace.WithResource(res),
	)
	otel.SetTracerProvider(tp)

	// 5. Metric exporter and MeterProvider
	metricExporter, err := otlpmetrichttp.New(ctx)
	if err != nil {
		slog.Warn("failed to initialize OTLP metric exporter; falling back to no-op telemetry", "err", err)
		_ = tp.Shutdown(ctx)
		return NewNoopTelemetry(), nil
	}

	mp := sdkmetric.NewMeterProvider(
		sdkmetric.WithReader(sdkmetric.NewPeriodicReader(metricExporter, sdkmetric.WithInterval(3*time.Second))),
		sdkmetric.WithResource(res),
	)
	otel.SetMeterProvider(mp)

	tracer := tp.Tracer(serviceName)
	meter := mp.Meter(serviceName)

	tel := &Telemetry{
		Enabled:        true,
		TracerProvider: tp,
		MeterProvider:  mp,
		Tracer:         tracer,
		Meter:          meter,
	}

	// 6. Initialize metric instruments
	if err := tel.initInstruments(meter); err != nil {
		slog.Warn("failed to initialize metric instruments", "err", err)
	}

	return tel, nil
}

// NewNoopTelemetry creates a Telemetry instance with no-op providers.
func NewNoopTelemetry() *Telemetry {
	tracer := nooptrace.NewTracerProvider().Tracer("dustwood-go")
	meter := noop.NewMeterProvider().Meter("dustwood-go")

	tel := &Telemetry{
		Enabled: false,
		Tracer:  tracer,
		Meter:   meter,
	}
	_ = tel.initInstruments(meter)
	return tel
}

func (t *Telemetry) initInstruments(meter metric.Meter) error {
	var err error

	t.mcpServerRequestCount, err = meter.Int64Counter(
		"mcp.server.request.count",
		metric.WithDescription("Count of MCP server requests received"),
		metric.WithUnit("{request}"),
	)
	if err != nil {
		return fmt.Errorf("mcp.server.request.count: %w", err)
	}

	t.mcpServerRequestDuration, err = meter.Float64Histogram(
		"mcp.server.request.duration",
		metric.WithDescription("Duration of MCP server requests"),
		metric.WithUnit("ms"),
	)
	if err != nil {
		return fmt.Errorf("mcp.server.request.duration: %w", err)
	}

	t.gameCommandCount, err = meter.Int64Counter(
		"dustwood.game.command.count",
		metric.WithDescription("Count of game commands executed"),
		metric.WithUnit("{command}"),
	)
	if err != nil {
		return fmt.Errorf("dustwood.game.command.count: %w", err)
	}

	t.gameCommandDuration, err = meter.Float64Histogram(
		"dustwood.game.command.duration",
		metric.WithDescription("Duration of game command execution"),
		metric.WithUnit("ms"),
	)
	if err != nil {
		return fmt.Errorf("dustwood.game.command.duration: %w", err)
	}

	t.turnGauge, err = meter.Int64Gauge(
		"dustwood.game.turn",
		metric.WithDescription("Current game turn number"),
		metric.WithUnit("{turn}"),
	)
	if err != nil {
		return fmt.Errorf("dustwood.game.turn: %w", err)
	}

	t.scoreGauge, err = meter.Int64Gauge(
		"dustwood.game.score",
		metric.WithDescription("Current player score"),
		metric.WithUnit("{point}"),
	)
	if err != nil {
		return fmt.Errorf("dustwood.game.score: %w", err)
	}

	t.thirstGauge, err = meter.Int64Gauge(
		"dustwood.game.thirst",
		metric.WithDescription("Current player thirst level"),
		metric.WithUnit("{thirst}"),
	)
	if err != nil {
		return fmt.Errorf("dustwood.game.thirst: %w", err)
	}

	t.horseThirstGauge, err = meter.Int64Gauge(
		"dustwood.game.horse_thirst",
		metric.WithDescription("Current horse thirst level"),
		metric.WithUnit("{thirst}"),
	)
	if err != nil {
		return fmt.Errorf("dustwood.game.horse_thirst: %w", err)
	}

	return nil
}

// Shutdown flushes and terminates both trace and metric providers within a timeout.
func (t *Telemetry) Shutdown(ctx context.Context) error {
	if !t.Enabled {
		return nil
	}

	shutdownCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()

	var errs []string
	if t.TracerProvider != nil {
		if err := t.TracerProvider.Shutdown(shutdownCtx); err != nil {
			errs = append(errs, fmt.Sprintf("trace shutdown: %v", err))
		}
	}
	if t.MeterProvider != nil {
		if err := t.MeterProvider.Shutdown(shutdownCtx); err != nil {
			errs = append(errs, fmt.Sprintf("metric shutdown: %v", err))
		}
	}

	if len(errs) > 0 {
		return fmt.Errorf("%s", strings.Join(errs, "; "))
	}
	return nil
}

// CanonicalVerb normalizes user/tool commands to a safe, fixed vocabulary.
// Unrecognized input is mapped to "unknown" and raw arguments/nouns are stripped.
func CanonicalVerb(cmd string) string {
	trimmed := strings.TrimSpace(cmd)
	if trimmed == "" {
		return "look"
	}
	verb, _ := splitCommand(trimmed)
	switch verb {
	case "N", "NORTH", "S", "SOUTH", "E", "EAST", "W", "WEST", "GO":
		return "go"
	case "LOOK", "L", "EXAMINE", "X", "SEARCH":
		return "look"
	case "TAKE", "GET":
		return "take"
	case "DROP", "D":
		return "drop"
	case "INVENTORY", "INV", "I":
		return "inventory"
	case "DRINK":
		return "drink"
	case "FILL":
		return "fill"
	case "WATER":
		return "water"
	case "LIGHT":
		return "light"
	case "FIX":
		return "fix"
	case "SADDLE", "PUT":
		return "saddle"
	case "MOUNT", "RIDE":
		return "ride"
	case "DISMOUNT":
		return "dismount"
	case "OPEN":
		return "open"
	case "SHOOT", "KILL":
		return "shoot"
	case "FREEZE", "WAIT":
		return "freeze"
	case "SCORE":
		return "score"
	case "SAVE":
		return "save"
	case "LOAD":
		return "load"
	case "QUIT", "Q":
		return "quit"
	case "CLIMB":
		return "climb"
	case "BURN":
		return "burn"
	case "FIRE":
		return "fire"
	case "HELP", "H", "?":
		return "help"
	case "[RESET]", "RESET":
		return "reset"
	default:
		return "unknown"
	}
}

// MCPReceivingMiddleware intercepts incoming MCP JSON-RPC requests, extracts W3C distributed trace
// context from metadata, records server spans with safe attributes, and emits request metrics.
func (t *Telemetry) MCPReceivingMiddleware() mcp.Middleware {
	return func(next mcp.MethodHandler) mcp.MethodHandler {
		return func(ctx context.Context, method string, req mcp.Request) (mcp.Result, error) {
			if t == nil || !t.Enabled {
				return next(ctx, method, req)
			}

			// 1. Extract W3C trace context from request _meta parameters
			carrier := propagation.MapCarrier{}
			if params := req.GetParams(); params != nil {
				if meta := params.GetMeta(); meta != nil {
					for k, v := range meta {
						if s, ok := v.(string); ok {
							carrier[strings.ToLower(k)] = s
						}
					}
				}
			}
			ctx = otel.GetTextMapPropagator().Extract(ctx, carrier)

			// 2. Determine tool name if tools/call
			var toolName string
			if method == "tools/call" {
				if toolParams, ok := req.GetParams().(*mcp.CallToolParams); ok && toolParams != nil {
					toolName = toolParams.Name
				}
			}

			// 3. Start server span
			spanName := "mcp.server " + method
			if toolName != "" {
				spanName = "mcp.tool " + toolName
			}

			spanAttrs := []attribute.KeyValue{
				attribute.String("rpc.system", "jsonrpc"),
				attribute.String("rpc.jsonrpc.version", "2.0"),
				attribute.String("rpc.method", method),
			}
			if toolName != "" {
				spanAttrs = append(spanAttrs, attribute.String("mcp.tool.name", toolName))
			}

			ctx, span := t.Tracer.Start(
				ctx,
				spanName,
				trace.WithSpanKind(trace.SpanKindServer),
				trace.WithAttributes(spanAttrs...),
			)
			defer span.End()

			start := time.Now()
			result, err := next(ctx, method, req)
			durationMs := float64(time.Since(start).Microseconds()) / 1000.0

			outcome := "success"
			if err != nil {
				outcome = "error"
				span.RecordError(err)
				span.SetStatus(codes.Error, err.Error())
			} else if callResult, ok := result.(*mcp.CallToolResult); ok && callResult != nil && callResult.IsError {
				// Tool execution failure (invalid argument, rejected reset, etc.)
				outcome = "error"
				span.SetStatus(codes.Error, "tool call returned error")
			}

			// 4. Record metrics
			metricAttrs := []attribute.KeyValue{
				attribute.String("rpc.method", method),
				attribute.String("outcome", outcome),
			}
			if toolName != "" {
				metricAttrs = append(metricAttrs, attribute.String("mcp.tool.name", toolName))
			}
			if t.mcpServerRequestCount != nil {
				t.mcpServerRequestCount.Add(ctx, 1, metric.WithAttributes(metricAttrs...))
			}
			if t.mcpServerRequestDuration != nil {
				t.mcpServerRequestDuration.Record(ctx, durationMs, metric.WithAttributes(metricAttrs...))
			}

			return result, err
		}
	}
}

// TraceGameInit traces initial game creation or resetting.
func (t *Telemetry) TraceGameInit(ctx context.Context, isReset bool, executionMode string, fn func()) {
	if t == nil || !t.Enabled {
		fn()
		return
	}

	spanName := "dustwood.game.init"
	if isReset {
		spanName = "dustwood.game.reset"
	}

	ctx, span := t.Tracer.Start(
		ctx,
		spanName,
		trace.WithAttributes(
			attribute.String("dustwood.execution_mode", executionMode),
			attribute.Int64("dustwood.turn", 0),
			attribute.Int64("dustwood.score", 0),
			attribute.Int64("dustwood.thirst", 0),
			attribute.Bool("dustwood.is_playing", true),
		),
	)
	defer span.End()

	fn()

	// Initialize gauges to starting state
	if t.turnGauge != nil {
		t.turnGauge.Record(ctx, 0)
	}
	if t.scoreGauge != nil {
		t.scoreGauge.Record(ctx, 0)
	}
	if t.thirstGauge != nil {
		t.thirstGauge.Record(ctx, 0)
	}
	if t.horseThirstGauge != nil {
		t.horseThirstGauge.Record(ctx, 0)
	}
}

// ExecuteCommandContext executes a game command within an OpenTelemetry span, capturing safe state
// metadata before and after execution, and emitting command metrics and gauge updates.
func ExecuteCommandContext(
	ctx context.Context,
	s *GameState,
	cmd string,
	executionMode string,
	tel *Telemetry,
) (string, GameSummary) {
	canonicalVerb := CanonicalVerb(cmd)

	if tel == nil || !tel.Enabled {
		return ExecuteCommand(s, cmd)
	}

	turnBefore := s.Turns
	scoreBefore := s.Score
	thirstBefore := s.Thirst
	horseThirstBefore := s.HorseThirst
	roomIDBefore := 0
	if s.CurrentRoom != nil {
		roomIDBefore = s.CurrentRoom.ID
	}

	spanAttrs := []attribute.KeyValue{
		attribute.String("dustwood.verb", canonicalVerb),
		attribute.String("dustwood.execution_mode", executionMode),
		attribute.Int64("dustwood.turn.before", int64(turnBefore)),
		attribute.Int64("dustwood.score.before", int64(scoreBefore)),
		attribute.Int64("dustwood.thirst.before", int64(thirstBefore)),
		attribute.Int64("dustwood.horse_thirst.before", int64(horseThirstBefore)),
		attribute.Int64("dustwood.room_id.before", int64(roomIDBefore)),
	}

	ctx, span := tel.Tracer.Start(
		ctx,
		"dustwood.game.command "+canonicalVerb,
		trace.WithAttributes(spanAttrs...),
	)
	defer span.End()

	start := time.Now()
	output, summary := ExecuteCommand(s, cmd)
	durationMs := float64(time.Since(start).Microseconds()) / 1000.0

	// Set state after execution
	span.SetAttributes(
		attribute.Int64("dustwood.turn.after", int64(summary.Turns)),
		attribute.Int64("dustwood.score.after", int64(summary.Score)),
		attribute.Int64("dustwood.thirst.after", int64(summary.Thirst)),
		attribute.Int64("dustwood.horse_thirst.after", int64(s.HorseThirst)),
		attribute.Int64("dustwood.room_id.after", int64(summary.RoomID)),
		attribute.String("dustwood.room_name", summary.RoomName),
		attribute.Bool("dustwood.is_playing", summary.IsPlaying),
		attribute.Bool("dustwood.game_over", !summary.IsPlaying),
	)

	// Record command metrics
	cmdAttrs := []attribute.KeyValue{
		attribute.String("dustwood.verb", canonicalVerb),
		attribute.String("dustwood.execution_mode", executionMode),
	}
	if tel.gameCommandCount != nil {
		tel.gameCommandCount.Add(ctx, 1, metric.WithAttributes(cmdAttrs...))
	}
	if tel.gameCommandDuration != nil {
		tel.gameCommandDuration.Record(ctx, durationMs, metric.WithAttributes(cmdAttrs...))
	}

	// Update latest-value gauges
	if tel.turnGauge != nil {
		tel.turnGauge.Record(ctx, int64(summary.Turns))
	}
	if tel.scoreGauge != nil {
		tel.scoreGauge.Record(ctx, int64(summary.Score))
	}
	if tel.thirstGauge != nil {
		tel.thirstGauge.Record(ctx, int64(summary.Thirst))
	}
	if tel.horseThirstGauge != nil {
		tel.horseThirstGauge.Record(ctx, int64(s.HorseThirst))
	}

	return output, summary
}
