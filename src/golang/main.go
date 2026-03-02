package main

import (
	"flag"
	"fmt"
	"log"
)

func main() {
	headless := flag.Bool("headless", false, "Run in headless mode (no raw terminal input)")
	mcpHTTP := flag.Bool("mcp-http", false, "Run MCP Streamable HTTP server")
	mcpAddr := flag.String("mcp-addr", "127.0.0.1:8765", "MCP listen address")
	mcpPath := flag.String("mcp-path", "/mcp", "MCP endpoint path")
	mcpToken := flag.String("mcp-token", "", "Bearer token for MCP requests (optional)")
	mcpJSON := flag.Bool("mcp-json-response", false, "Force JSON responses instead of SSE")
	mcpStateless := flag.Bool("mcp-stateless", false, "Run MCP server in stateless mode (no sessions/SSE)")
	seedFlag := flag.Int64("seed", -1, "Deterministic game seed (optional)")
	turnLimitFlag := flag.Int("turns", 25, "Set the turn limit")
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
		fmt.Printf("  --seed <n>           Set the random seed\n")
		fmt.Printf("  --autosave           Enable autosave feature\n")
		fmt.Printf("  --autosave-interval  Turns between autosaves (default: 5)\n")
		fmt.Printf("  --autosave-path      Autosave file path (default: data/autosave.db)\n")
	}

	flag.Parse()

	var seed *int64
	if *seedFlag >= 0 {
		seed = seedFlag
	}

	if *mcpHTTP {
		if len(origins) == 0 {
			origins = append(origins, "http://localhost", "http://127.0.0.1")
		}
		server := NewMCPServer(seed, *turnLimitFlag)
		// Propagate autosave settings to server game instance
		server.game.AutosaveEnabled = *autosaveEnabled
		server.game.AutosaveInterval = *autosaveInterval
		server.game.AutosavePath = *autosavePath

		if err := RunMCPHTTP(server, *mcpAddr, *mcpPath, origins, *mcpToken, *mcpJSON, *mcpStateless); err != nil {
			log.Fatal(err)
		}
		return
	}

	s := NewGame(seed, *turnLimitFlag, nil)
	s.IsHeadless = *headless
	s.AutosaveEnabled = *autosaveEnabled
	s.AutosaveInterval = *autosaveInterval
	s.AutosavePath = *autosavePath

	for s.IsPlaying {
		cmd := customReadLn(s, "> ")
		processCommand(s, cmd)
	}
	outPrintln(s)
	outPrintf(s, "🏆 Final score: %d\n", s.Score)
}
