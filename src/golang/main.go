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
	var origins stringSlice
	flag.Var(&origins, "mcp-origin", "Allowed Origin for MCP requests (repeatable)")

	flag.Usage = func() {
		fmt.Printf("Usage: dustwood [options]\n\n")
		fmt.Printf("Options:\n")
		fmt.Printf("  -h, --h, --help Show this help message\n")
		fmt.Printf("  --headless      Run in headless mode\n")
		fmt.Printf("  --turns <n>     Set the turn limit (default: 25)\n")
		fmt.Printf("  --seed <n>      Set the random seed\n")
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
		server := NewMCPServer(seed)
		if err := RunMCPHTTP(server, *mcpAddr, *mcpPath, origins, *mcpToken, *mcpJSON, *mcpStateless); err != nil {
			log.Fatal(err)
		}
		return
	}

	s := NewGame(seed, nil)
	s.IsHeadless = *headless

	turnLimit := *turnLimitFlag
	for s.IsPlaying {
		cmd := customReadLn(s, "> ")
		processCommand(s, cmd)
		if s.Turns >= turnLimit && s.IsPlaying {
			outPrintln(s)
			outPrintln(s, "⏳ You have taken too long. The sun dips below the horizon.")
			outPrintln(s, "GAME OVER.")
			s.IsPlaying = false
		}
	}
	outPrintln(s)
	outPrintf(s, "🏆 Final score: %d\n", s.Score)
}
