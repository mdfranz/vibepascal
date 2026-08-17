package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"os/signal"
	"syscall"
	"time"
)

func main() {
	// Ensure slog outputs to stderr, not stdout (required for MCP stdio transport)
	slog.SetDefault(slog.New(slog.NewJSONHandler(os.Stderr, &slog.HandlerOptions{
		Level: slog.LevelInfo,
	})))

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()

	headless := flag.Bool("headless", false, "Run in headless mode (no raw terminal input)")
	mcpHTTP := flag.Bool("mcp-http", false, "Run MCP Streamable HTTP server")
	mcpAddr := flag.String("mcp-addr", "127.0.0.1:8765", "MCP listen address")
	mcpPath := flag.String("mcp-path", "/mcp", "MCP endpoint path")
	mcpToken := flag.String("mcp-token", "", "Bearer token for MCP requests (optional)")
	mcpJSON := flag.Bool("mcp-json-response", false, "Force JSON responses instead of SSE")
	mcpStateless := flag.Bool("mcp-stateless", false, "Run MCP server in stateless mode (no sessions/SSE)")
	seedFlag := flag.Int64("seed", -1, "Deterministic game seed (optional)")
	turnLimitFlag := flag.Int("turns", 25, "Set the turn limit")
	allowRestart := flag.Bool("allow-restart", false, "Allow reset_game/command(reset=true) after GAME OVER (default: one attempt per server process)")
	autosaveEnabled := flag.Bool("autosave", false, "Enable autosave")
	autosaveInterval := flag.Int("autosave-interval", 5, "Turns between autosaves")
	autosavePath := flag.String("autosave-path", "data/autosave.db", "Path to autosave file")
	var origins stringSlice
	flag.Var(&origins, "mcp-origin", "Allowed Origin for MCP requests (repeatable)")

	flag.Usage = func() {
		fmt.Printf("Usage: dustwood [options]\n\n")
		fmt.Printf("Options:\n")
		fmt.Printf("  -h, --h, --help      Show this help message\n")
		fmt.Printf("  --headless           Run in headless mode\n")
		fmt.Printf("  --turns <n>          Set the turn limit (default: 25)\n")
		fmt.Printf("  --allow-restart      Allow reset_game/reset=true after GAME OVER (default: one attempt per process)\n")
		fmt.Printf("  --seed <n>           Set the random seed\n")
		fmt.Printf("  --autosave           Enable autosave feature\n")
		fmt.Printf("  --autosave-interval  Turns between autosaves (default: 5)\n")
		fmt.Printf("  --autosave-path      Autosave file path (default: data/autosave.db)\n")
		fmt.Printf("  --mcp-http           Run MCP Streamable HTTP server\n")
		fmt.Printf("  --mcp-addr <addr>    MCP listen address (default: 127.0.0.1:8765)\n")
		fmt.Printf("  --mcp-path <path>    MCP endpoint path (default: /mcp)\n")
		fmt.Printf("  --mcp-token <tok>    Bearer token for MCP requests (optional)\n")
		fmt.Printf("  --mcp-json-response  Force JSON responses instead of SSE\n")
		fmt.Printf("  --mcp-stateless      Run MCP server in stateless mode\n")
		fmt.Printf("  --mcp-origin <orig>  Allowed Origin for MCP requests (repeatable)\n")
	}

	flag.Parse()

	tel, err := InitTelemetry(ctx)
	if err != nil {
		slog.Warn("telemetry initialization note", "err", err)
	}
	defer func() {
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		if err := tel.Shutdown(shutdownCtx); err != nil {
			slog.Warn("telemetry shutdown error", "err", err)
		}
	}()

	var seed *int64
	if *seedFlag >= 0 {
		seed = seedFlag
	}

	if *mcpHTTP {
		if len(origins) == 0 {
			origins = append(origins, "http://localhost", "http://127.0.0.1")
		}
		server := NewMCPServer(seed, *turnLimitFlag, *allowRestart, tel)
		// Propagate autosave settings to server game instance
		server.game.AutosaveEnabled = *autosaveEnabled
		server.game.AutosaveInterval = *autosaveInterval
		server.game.AutosavePath = *autosavePath

		if err := RunMCPHTTP(ctx, server, *mcpAddr, *mcpPath, origins, *mcpToken, *mcpJSON, *mcpStateless); err != nil {
			slog.Error("MCP HTTP server stopped with error", "err", err)
			os.Exit(1)
		}
		return
	}

	cliCtx, sessionSpan := tel.Tracer.Start(ctx, "dustwood.cli_session")
	defer sessionSpan.End()

	var s *GameState
	tel.TraceGameInit(cliCtx, false, "cli", func() {
		s = NewGame(seed, *turnLimitFlag, nil)
	})
	s.IsHeadless = *headless
	s.AutosaveEnabled = *autosaveEnabled
	s.AutosaveInterval = *autosaveInterval
	s.AutosavePath = *autosavePath

	for s.IsPlaying {
		select {
		case <-ctx.Done():
			break
		default:
		}
		cmd := customReadLn(s, "> ")
		ExecuteCommandContext(cliCtx, s, cmd, "cli", tel)
	}
	outPrintln(s)
	outPrintf(s, "🏆 Final score: %d\n", s.Score)
}
