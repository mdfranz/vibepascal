package main

import (
	"bytes"
	"context"
	"io"
	"log/slog"
	"net/http"
	"strings"
	"sync"

	"github.com/modelcontextprotocol/go-sdk/mcp"
)

type CommandInput struct {
	Command string `json:"command" jsonschema:"Game command to execute"`
	Reset   bool   `json:"reset,omitempty" jsonschema:"Reset the game before executing the command"`
	Seed    *int64 `json:"seed,omitempty" jsonschema:"Seed to use when resetting the game"`
}

type CommandOutput struct {
	Output string      `json:"output" jsonschema:"Raw game output"`
	State  GameSummary `json:"state" jsonschema:"Summary of the current game state"`
}

type MCPServer struct {
	mu          sync.Mutex
	game        *GameState
	defaultSeed *int64
}

func NewMCPServer(seed *int64) *MCPServer {
	return &MCPServer{
		game:        NewGame(seed, io.Discard),
		defaultSeed: seed,
	}
}

func ExecuteCommand(s *GameState, cmd string) (string, GameSummary) {
	var buf bytes.Buffer
	prevOut := s.Out
	s.Out = &buf
	defer func() {
		s.Out = prevOut
	}()

	trimmed := strings.TrimSpace(cmd)
	if trimmed == "" {
		look(s)
	} else {
		processCommand(s, trimmed)
	}

	return buf.String(), SummarizeState(s)
}

func (s *MCPServer) HandleCommand(_ context.Context, _ *mcp.CallToolRequest, input *CommandInput) (*mcp.CallToolResult, *CommandOutput, error) {
	if input == nil {
		input = &CommandInput{}
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	if input.Reset {
		seed := s.defaultSeed
		if input.Seed != nil {
			seed = input.Seed
		}
		var buf bytes.Buffer
		s.game = NewGame(seed, &buf)
		return nil, &CommandOutput{
			Output: buf.String(),
			State:  SummarizeState(s.game),
		}, nil
	}

	output, summary := ExecuteCommand(s.game, input.Command)
	return nil, &CommandOutput{
		Output: output,
		State:  summary,
	}, nil
}

func RunMCPHTTP(server *MCPServer, addr, path string, origins []string, token string, jsonResponse bool, stateless bool) error {
	mcpServer := mcp.NewServer(&mcp.Implementation{
		Name:    "dustwood-go",
		Version: "v1.0.0",
	}, nil)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "command",
		Description: "Send a command to the Dustwood game and return output plus state summary.",
	}, server.HandleCommand)

	if !strings.HasPrefix(path, "/") {
		path = "/" + path
	}

	handler := mcp.NewStreamableHTTPHandler(func(_ *http.Request) *mcp.Server {
		return mcpServer
	}, &mcp.StreamableHTTPOptions{
		Stateless:                  stateless,
		JSONResponse:               jsonResponse,
		Logger:                     slog.Default(),
		DisableLocalhostProtection: false,
	})

	originSet := map[string]struct{}{}
	for _, origin := range origins {
		originSet[origin] = struct{}{}
	}

	guarded := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if !isAllowedOrigin(r, originSet) {
			http.Error(w, "Forbidden origin", http.StatusForbidden)
			return
		}
		if token != "" && r.Header.Get("Authorization") != "Bearer "+token {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}
		handler.ServeHTTP(w, r)
	})

	mux := http.NewServeMux()
	mux.Handle(path, guarded)

	serverHTTP := &http.Server{
		Addr:    addr,
		Handler: mux,
	}
	return serverHTTP.ListenAndServe()
}

func isAllowedOrigin(r *http.Request, allowed map[string]struct{}) bool {
	origin := r.Header.Get("Origin")
	if origin == "" {
		return true
	}
	_, ok := allowed[origin]
	return ok
}
