package main

import (
	"context"
	"strings"
	"testing"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/propagation"
	sdkmetric "go.opentelemetry.io/otel/sdk/metric"
	"go.opentelemetry.io/otel/sdk/metric/metricdata"
	sdktrace "go.opentelemetry.io/otel/sdk/trace"
	"go.opentelemetry.io/otel/sdk/trace/tracetest"
	"go.opentelemetry.io/otel/trace"
)

func setupTestTelemetry(t *testing.T) (*Telemetry, *tracetest.InMemoryExporter, *sdkmetric.ManualReader) {
	t.Helper()

	spanExporter := tracetest.NewInMemoryExporter()
	tp := sdktrace.NewTracerProvider(
		sdktrace.WithSyncer(spanExporter),
	)

	metricReader := sdkmetric.NewManualReader()
	mp := sdkmetric.NewMeterProvider(
		sdkmetric.WithReader(metricReader),
	)

	tracer := tp.Tracer("dustwood-test")
	meter := mp.Meter("dustwood-test")

	tel := &Telemetry{
		Enabled:        true,
		TracerProvider: tp,
		MeterProvider:  mp,
		Tracer:         tracer,
		Meter:          meter,
	}
	if err := tel.initInstruments(meter); err != nil {
		t.Fatalf("failed to init instruments: %v", err)
	}

	otel.SetTextMapPropagator(propagation.NewCompositeTextMapPropagator(
		propagation.TraceContext{},
		propagation.Baggage{},
	))

	return tel, spanExporter, metricReader
}

func TestCanonicalVerb(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"", "look"},
		{"  ", "look"},
		{"look", "look"},
		{"LOOK", "look"},
		{"l", "look"},
		{"examine rock", "look"},
		{"X LAMP", "look"},
		{"search", "look"},
		{"go north", "go"},
		{"N", "go"},
		{"south", "go"},
		{"e", "go"},
		{"west", "go"},
		{"take canteen", "take"},
		{"GET MATCHES", "take"},
		{"drop lamp", "drop"},
		{"d saddle", "drop"},
		{"inv", "inventory"},
		{"inventory", "inventory"},
		{"i", "inventory"},
		{"drink", "drink"},
		{"fill", "fill"},
		{"water horse", "water"},
		{"light lamp", "light"},
		{"fix pump", "fix"},
		{"saddle horse", "saddle"},
		{"ride", "ride"},
		{"mount", "ride"},
		{"dismount", "dismount"},
		{"open box", "open"},
		{"shoot outlaw", "shoot"},
		{"freeze", "freeze"},
		{"wait", "freeze"},
		{"score", "score"},
		{"save data/save.db", "save"},
		{"load data/save.db", "load"},
		{"quit", "quit"},
		{"q", "quit"},
		{"climb", "climb"},
		{"burn book", "burn"},
		{"fire", "fire"},
		{"help", "help"},
		{"?", "help"},
		{"[reset]", "reset"},
		{"xyzzy", "unknown"},
		{"foobar 123", "unknown"},
	}

	for _, tt := range tests {
		got := CanonicalVerb(tt.input)
		if got != tt.expected {
			t.Errorf("CanonicalVerb(%q) = %q, expected %q", tt.input, got, tt.expected)
		}
	}
}

func TestMCPReceivingMiddleware_TraceParentPropagation(t *testing.T) {
	tel, spanExporter, _ := setupTestTelemetry(t)

	// Simulate incoming W3C traceparent
	knownTraceID := "4bf92f3577b34da6a3ce929d0e0e4736"
	knownSpanID := "00f067aa0ba902b7"
	traceparent := "00-" + knownTraceID + "-" + knownSpanID + "-01"

	req := &mcp.ServerRequest[*mcp.CallToolParams]{
		Params: &mcp.CallToolParams{
			Meta: mcp.Meta{
				"traceparent": traceparent,
			},
			Name: "look",
		},
	}

	middleware := tel.MCPReceivingMiddleware()
	handler := middleware(func(ctx context.Context, method string, r mcp.Request) (mcp.Result, error) {
		span := trace.SpanFromContext(ctx)
		if !span.SpanContext().IsValid() {
			t.Error("expected valid span context inside handler")
		}
		if span.SpanContext().TraceID().String() != knownTraceID {
			t.Errorf("expected TraceID %s, got %s", knownTraceID, span.SpanContext().TraceID().String())
		}
		return &mcp.CallToolResult{IsError: false}, nil
	})

	_, err := handler(context.Background(), "tools/call", req)
	if err != nil {
		t.Fatalf("handler failed: %v", err)
	}

	spans := spanExporter.GetSpans()
	if len(spans) != 1 {
		t.Fatalf("expected 1 span, got %d", len(spans))
	}

	span := spans[0]
	if span.Name != "mcp.tool look" {
		t.Errorf("expected span name 'mcp.tool look', got %q", span.Name)
	}
	if span.SpanContext.TraceID().String() != knownTraceID {
		t.Errorf("expected span TraceID %s, got %s", knownTraceID, span.SpanContext.TraceID().String())
	}
	if span.Parent.SpanID().String() != knownSpanID {
		t.Errorf("expected parent SpanID %s, got %s", knownSpanID, span.Parent.SpanID().String())
	}

	// Verify safe attributes: rpc.system, rpc.jsonrpc.version, rpc.method, mcp.tool.name
	var hasRPCSystem, hasMethod, hasToolName bool
	for _, attr := range span.Attributes {
		switch attr.Key {
		case "rpc.system":
			if attr.Value.AsString() == "jsonrpc" {
				hasRPCSystem = true
			}
		case "rpc.method":
			if attr.Value.AsString() == "tools/call" {
				hasMethod = true
			}
		case "mcp.tool.name":
			if attr.Value.AsString() == "look" {
				hasToolName = true
			}
		}
	}
	if !hasRPCSystem || !hasMethod || !hasToolName {
		t.Errorf("missing expected span attributes: system=%v, method=%v, toolName=%v", hasRPCSystem, hasMethod, hasToolName)
	}
}

func TestExecuteCommandContext_TracingAndGauges(t *testing.T) {
	tel, spanExporter, metricReader := setupTestTelemetry(t)

	game := NewGame(nil, 25, nil)
	ctx := context.Background()

	output, summary := ExecuteCommandContext(ctx, game, "look", "mcp", tel)
	if !strings.Contains(output, "Dustwood") && !strings.Contains(summary.RoomName, "Dustwood") && summary.RoomID == 0 {
		t.Errorf("unexpected game state after look: %v", summary)
	}

	spans := spanExporter.GetSpans()
	if len(spans) != 1 {
		t.Fatalf("expected 1 span, got %d", len(spans))
	}

	span := spans[0]
	if span.Name != "dustwood.game.command look" {
		t.Errorf("expected span name 'dustwood.game.command look', got %q", span.Name)
	}

	// Verify attributes do not contain raw game output
	for _, attr := range span.Attributes {
		if strings.Contains(attr.Value.AsString(), output) && len(output) > 10 {
			t.Errorf("raw output leaked into telemetry attribute %s", attr.Key)
		}
	}

	// Verify metrics
	var rm metricdata.ResourceMetrics
	if err := metricReader.Collect(ctx, &rm); err != nil {
		t.Fatalf("failed to collect metrics: %v", err)
	}

	var foundCommandCount, foundTurnGauge bool
	for _, sm := range rm.ScopeMetrics {
		for _, m := range sm.Metrics {
			switch m.Name {
			case "dustwood.game.command.count":
				foundCommandCount = true
			case "dustwood.game.turn":
				foundTurnGauge = true
			}
		}
	}
	if !foundCommandCount {
		t.Error("expected dustwood.game.command.count metric")
	}
	if !foundTurnGauge {
		t.Error("expected dustwood.game.turn gauge metric")
	}
}

func TestTraceGameInit(t *testing.T) {
	tel, spanExporter, _ := setupTestTelemetry(t)

	ctx := context.Background()
	tel.TraceGameInit(ctx, false, "cli", func() {
		// simulation of game init
	})

	spans := spanExporter.GetSpans()
	if len(spans) != 1 {
		t.Fatalf("expected 1 span, got %d", len(spans))
	}

	span := spans[0]
	if span.Name != "dustwood.game.init" {
		t.Errorf("expected 'dustwood.game.init', got %q", span.Name)
	}

	// Reset trace
	spanExporter.Reset()
	tel.TraceGameInit(ctx, true, "mcp", func() {
		// simulation of game reset
	})

	spans = spanExporter.GetSpans()
	if len(spans) != 1 {
		t.Fatalf("expected 1 span, got %d", len(spans))
	}
	if spans[0].Name != "dustwood.game.reset" {
		t.Errorf("expected 'dustwood.game.reset', got %q", spans[0].Name)
	}
}

func TestNoopTelemetry_DoesNotPanic(t *testing.T) {
	noopTel := NewNoopTelemetry()
	ctx := context.Background()

	game := NewGame(nil, 25, nil)
	output, summary := ExecuteCommandContext(ctx, game, "look", "mcp", noopTel)
	if output == "" || summary.RoomName == "" {
		t.Errorf("expected normal game execution under no-op telemetry")
	}

	noopTel.TraceGameInit(ctx, false, "cli", func() {})
	if err := noopTel.Shutdown(ctx); err != nil {
		t.Errorf("unexpected error on no-op shutdown: %v", err)
	}
}
