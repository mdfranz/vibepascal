package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"strings"
	"sync"
	"time"

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

// Tool input types for decomposed action tools
type GoInput struct {
	Direction string `json:"direction" jsonschema:"Compass direction: north, south, east, or west"`
}

type TakeInput struct {
	Item string `json:"item" jsonschema:"Name of the item to pick up"`
}

type DropInput struct {
	Item string `json:"item" jsonschema:"Name of the item to drop"`
}

type ResetGameInput struct {
	Seed *int64 `json:"seed,omitempty" jsonschema:"Optional seed for deterministic gameplay"`
}

type EmptyInput struct {
	Unused *string `json:"unused,omitempty" jsonschema:"Unused parameter"`
}

const dustwoodSystemPrompt = `You are playing Dustwood, a text-based adventure game set in a dying Western frontier town.

## Game Overview
You wake up in a distant frontier town, parched and desperate. Your goal is to survive, quench your thirst, and escape before nightfall. The town is dangerous, with hazards like the desert, wild horses, and thirst.

## Valid Commands
- **look** - Examine your surroundings (room description, items, exits)
- **go [direction]** - Move in a compass direction (north, south, east, west)
- **take [item]** - Pick up an item in the current room
- **drop [item]** - Drop an item from your inventory
- **inventory** - List items you are carrying
- **drink** - Drink from your canteen to reduce thirst
- **water [horse]** - Water your horse to reduce its thirst (requires water)
- **light [lamp]** - Light your lamp to see in dark areas
- **ride** - Mount your horse
- **dismount** - Dismount from your horse
- **score** - Display your current score

## Critical Survival Tips
1. **Thirst is deadly** - You gain thirst every turn. The desert is extremely dangerous and increases thirst rapidly. Find water!
2. **Water sources** - Look for streams and fill your canteen at water sources.
3. **Horse care** - Your horse also gets thirsty. Water it periodically to keep it healthy.
4. **Darkness** - Some rooms are pitch black and dangerous. Light your lamp to navigate safely.
5. **Navigation** - Explore methodically. Use 'look' to check exits and items before moving.
6. **Score** - Maximize your score by exploring, finding items, and surviving longer.

## Strategy for Success
1. Start by looking around to get your bearings
2. Find a water source quickly and fill your canteen
3. Explore the town cautiously, avoiding the desert
4. Keep your lamp lit when entering dark areas
5. Monitor your thirst gauge and the turn limit
6. Return to safety before nightfall (turn limit)

Use the available tools and MCP resources (game://state, game://room, game://inventory) to track your progress.`

type MCPServer struct {
	mu           sync.Mutex
	game         *GameState
	defaultSeed  *int64
	turnLimit    int
	allowRestart bool
	telemetry    *Telemetry
}

// NewMCPServer constructs a game server. When allowRestart is false (the default via the
// --allow-restart CLI flag), reset_game/command(reset=true) are rejected once the current game
// has already ended (IsPlaying=false) - a real death or day/night timeout - so a benchmark run
// gets exactly one continuous playthrough instead of a model being able to retry after failing.
// The mandatory initial bootstrap reset (sent while IsPlaying is still true, before anything has
// happened) is unaffected either way.
func NewMCPServer(seed *int64, turnLimit int, allowRestart bool, tel *Telemetry) *MCPServer {
	if tel == nil {
		tel = NewNoopTelemetry()
	}
	return &MCPServer{
		game:         NewGame(seed, turnLimit, io.Discard),
		defaultSeed:  seed,
		turnLimit:    turnLimit,
		allowRestart: allowRestart,
		telemetry:    tel,
	}
}

// restartBlocked reports whether a reset attempt should be rejected: restarts are disabled and
// the current game has already ended. Must be called with s.mu held.
func (s *MCPServer) restartBlocked() bool {
	return !s.allowRestart && s.game != nil && !s.game.IsPlaying
}

// validateItemName validates that an item name is non-empty and not too long
func validateItemName(name string) error {
	trimmed := strings.TrimSpace(name)
	if len(trimmed) == 0 {
		return fmt.Errorf("item name cannot be empty")
	}
	if len(trimmed) > 32 {
		return fmt.Errorf("item name too long (max 32 characters)")
	}
	return nil
}

// Handler for the "look" tool
func (s *MCPServer) HandleLook(ctx context.Context, _ *mcp.CallToolRequest, _ *EmptyInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, "look", "mcp", s.telemetry)
	slog.Info("tool", "name", "look", "room", summary.RoomName, "turn", summary.Turns)

	// summary.IsPlaying=false is a legitimate terminal game state (win, death, or timeout), not a
	// tool-call error - IsError must stay false so MCP clients don't treat GAME OVER as something
	// to retry/fix (see logfire_results/openrouter-deepseek-vs-gemini-2026-08-06.md).
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "go" tool
func (s *MCPServer) HandleGo(ctx context.Context, _ *mcp.CallToolRequest, input *GoInput) (*mcp.CallToolResult, *CommandOutput, error) {
	dir := strings.ToLower(strings.TrimSpace(input.Direction))
	validDirs := map[string]string{
		"north": "N", "south": "S", "east": "E", "west": "W",
		"n": "N", "s": "S", "e": "E", "w": "W",
	}
	verb, ok := validDirs[dir]
	if !ok {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{
			Output: fmt.Sprintf("Unknown direction: %q. Use north, south, east, or west.", input.Direction),
			State:  GameSummary{},
		}, nil
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, verb, "mcp", s.telemetry)
	slog.Info("tool", "name", "go", "direction", dir, "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "take" tool
func (s *MCPServer) HandleTake(ctx context.Context, _ *mcp.CallToolRequest, input *TakeInput) (*mcp.CallToolResult, *CommandOutput, error) {
	if err := validateItemName(input.Item); err != nil {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{
			Output: err.Error(),
			State:  GameSummary{},
		}, nil
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, fmt.Sprintf("take %s", strings.TrimSpace(input.Item)), "mcp", s.telemetry)
	slog.Info("tool", "name", "take", "item", input.Item, "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "drop" tool
func (s *MCPServer) HandleDrop(ctx context.Context, _ *mcp.CallToolRequest, input *DropInput) (*mcp.CallToolResult, *CommandOutput, error) {
	if err := validateItemName(input.Item); err != nil {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{
			Output: err.Error(),
			State:  GameSummary{},
		}, nil
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, fmt.Sprintf("drop %s", strings.TrimSpace(input.Item)), "mcp", s.telemetry)
	slog.Info("tool", "name", "drop", "item", input.Item, "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "inventory" tool
func (s *MCPServer) HandleInventory(ctx context.Context, _ *mcp.CallToolRequest, _ *EmptyInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, "inv", "mcp", s.telemetry)
	slog.Info("tool", "name", "inventory", "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "drink" tool
func (s *MCPServer) HandleDrink(ctx context.Context, _ *mcp.CallToolRequest, _ *EmptyInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, "drink", "mcp", s.telemetry)
	slog.Info("tool", "name", "drink", "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "water_horse" tool
func (s *MCPServer) HandleWaterHorse(ctx context.Context, _ *mcp.CallToolRequest, _ *EmptyInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, "water horse", "mcp", s.telemetry)
	slog.Info("tool", "name", "water_horse", "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "light" tool
func (s *MCPServer) HandleLight(ctx context.Context, _ *mcp.CallToolRequest, _ *EmptyInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, "light lamp", "mcp", s.telemetry)
	slog.Info("tool", "name", "light", "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "score" tool
func (s *MCPServer) HandleScore(ctx context.Context, _ *mcp.CallToolRequest, _ *EmptyInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommandContext(ctx, s.game, "score", "mcp", s.telemetry)
	slog.Info("tool", "name", "score", "room", summary.RoomName, "turn", summary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "reset_game" tool
func (s *MCPServer) HandleResetGame(ctx context.Context, _ *mcp.CallToolRequest, input *ResetGameInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.restartBlocked() {
		slog.Info("tool", "name", "reset_game", "rejected", "restarts disabled after game over")
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{
			Output: "Restarts are disabled on this server (--allow-restart not set). The game has ended - this was your one attempt.",
			State:  SummarizeState(s.game),
		}, nil
	}

	seed := s.defaultSeed
	if input != nil && input.Seed != nil {
		seed = input.Seed
	}

	var buf bytes.Buffer
	s.telemetry.TraceGameInit(ctx, true, "mcp", func() {
		s.game = NewGame(seed, s.turnLimit, &buf)
	})
	resetSummary := SummarizeState(s.game)

	slog.Info("tool", "name", "reset_game", "room", resetSummary.RoomName, "turn", resetSummary.Turns)

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{Output: buf.String(), State: resetSummary}, nil
}

func ExecuteCommand(s *GameState, cmd string) (string, GameSummary) {
	var buf bytes.Buffer
	prevOut := s.Out
	s.Out = &buf
	defer func() {
		s.Out = prevOut
	}()

	// A finished game is immutable. Reset requests are handled separately by the MCP
	// handlers, where restart policy is enforced; ordinary commands must not be able
	// to move, collect items, or change the score after GAME OVER.
	if !s.IsPlaying {
		outPrintln(s, "GAME OVER. No further actions are accepted.")
		return buf.String(), SummarizeState(s)
	}

	trimmed := strings.TrimSpace(cmd)
	if trimmed == "" {
		look(s)
	} else {
		processCommand(s, trimmed)
	}

	if s.IsPlaying && s.TurnLimit > 0 && s.Turns >= s.TurnLimit {
		outPrintln(s)
		outPrintln(s, "⏳ You have taken too long. The sun dips below the horizon.")
		outPrintln(s, "GAME OVER.")
		s.IsPlaying = false
	}

	return buf.String(), SummarizeState(s)
}

func (s *MCPServer) HandleCommand(ctx context.Context, _ *mcp.CallToolRequest, input *CommandInput) (*mcp.CallToolResult, *CommandOutput, error) {
	if input == nil {
		input = &CommandInput{}
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	if input.Reset {
		if s.restartBlocked() {
			slog.Info("command", "cmd", "[reset]", "rejected", "restarts disabled after game over")
			result := &mcp.CallToolResult{IsError: true}
			return result, &CommandOutput{
				Output: "Restarts are disabled on this server (--allow-restart not set). The game has ended - this was your one attempt.",
				State:  SummarizeState(s.game),
			}, nil
		}

		seed := s.defaultSeed
		if input.Seed != nil {
			seed = input.Seed
		}
		var buf bytes.Buffer
		s.telemetry.TraceGameInit(ctx, true, "mcp", func() {
			s.game = NewGame(seed, s.turnLimit, &buf)
		})
		resetSummary := SummarizeState(s.game)
		slog.Info("command",
			"cmd", "[reset]",
			"room", resetSummary.RoomName,
			"turn", resetSummary.Turns,
			"score", resetSummary.Score,
			"thirst", resetSummary.Thirst,
			"playing", resetSummary.IsPlaying,
		)

		if !resetSummary.IsPlaying {
			slog.Info("game ended after reset")
		}

		// See comment in HandleLook: a natural game end is not a tool-call error.
		return nil, &CommandOutput{
			Output: buf.String(),
			State:  resetSummary,
		}, nil
	}

	output, summary := ExecuteCommandContext(ctx, s.game, input.Command, "mcp", s.telemetry)
	slog.Info("command",
		"cmd", input.Command,
		"room", summary.RoomName,
		"turn", summary.Turns,
		"score", summary.Score,
		"thirst", summary.Thirst,
		"playing", summary.IsPlaying,
	)

	if !summary.IsPlaying {
		slog.Info("game ended")
	}

	// See comment in HandleLook: a natural game end is not a tool-call error.
	return nil, &CommandOutput{
		Output: output,
		State:  summary,
	}, nil
}

func RunMCPHTTP(ctx context.Context, server *MCPServer, addr, path string, origins []string, token string, jsonResponse bool, stateless bool) error {
	mcpServer := createMCPServer(server)

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
	mux.HandleFunc("GET /health", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte(`{"status":"ok","service":"dustwood-go"}`))
	})
	mux.Handle(path, guarded)

	serverHTTP := &http.Server{
		Addr:    addr,
		Handler: mux,
	}

	// Handle graceful shutdown on context cancellation
	shutdownErr := make(chan error, 1)
	go func() {
		<-ctx.Done()
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		slog.Info("shutting down HTTP server...")
		shutdownErr <- serverHTTP.Shutdown(shutdownCtx)
	}()

	slog.Info("listening",
		"addr", addr,
		"path", path,
		"stateless", stateless,
		"json_response", jsonResponse,
	)
	err := serverHTTP.ListenAndServe()
	if err != nil && err != http.ErrServerClosed {
		return err
	}
	if err := <-shutdownErr; err != nil && err != http.ErrServerClosed {
		return err
	}
	return nil
}

func RunMCPStdio(ctx context.Context, server *MCPServer) error {
	mcpServer := createMCPServer(server)
	return mcpServer.Run(ctx, &mcp.StdioTransport{})
}

func createMCPServer(server *MCPServer) *mcp.Server {
	mcpServer := mcp.NewServer(&mcp.Implementation{
		Name:    "dustwood-go",
		Version: "v1.0.0",
	}, nil)

	// Add OpenTelemetry receiving middleware if enabled
	if server != nil && server.telemetry != nil && server.telemetry.Enabled {
		mcpServer.AddReceivingMiddleware(server.telemetry.MCPReceivingMiddleware())
	}

	// Register generic command tool
	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "command",
		Description: "Send a command to the Dustwood game and return output plus state summary.",
	}, server.HandleCommand)

	// Register decomposed action tools
	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "look",
		Description: "Look around the current room to see exits, items, and description.",
	}, server.HandleLook)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "go",
		Description: "Move in a compass direction (north, south, east, west). Returns room description and updated game state.",
	}, server.HandleGo)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "take",
		Description: "Pick up a named item in the current room and add it to your inventory.",
	}, server.HandleTake)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "drop",
		Description: "Drop an item from your inventory into the current room.",
	}, server.HandleDrop)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "inventory",
		Description: "List all items you are carrying and view your status.",
	}, server.HandleInventory)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "drink",
		Description: "Drink from your canteen to reduce thirst. Requires water in your canteen.",
	}, server.HandleDrink)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "water_horse",
		Description: "Give water to your horse to reduce its thirst. Requires water in your canteen.",
	}, server.HandleWaterHorse)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "light",
		Description: "Light your lamp to illuminate dark areas. Consumes fuel and affects visibility.",
	}, server.HandleLight)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "score",
		Description: "Display your current score and game statistics.",
	}, server.HandleScore)

	mcp.AddTool(mcpServer, &mcp.Tool{
		Name:        "reset_game",
		Description: "Start a new game, optionally with a specific seed for deterministic gameplay.",
	}, server.HandleResetGame)

	// Register MCP Resources
	mcpServer.AddResource(&mcp.Resource{
		URI:         "game://state",
		Name:        "Game State",
		Description: "Current game state: room, score, turns, thirst, inventory flags.",
		MIMEType:    "application/json",
	}, func(_ context.Context, req *mcp.ReadResourceRequest) (*mcp.ReadResourceResult, error) {
		server.mu.Lock()
		defer server.mu.Unlock()
		stateJSON := SummarizeStateJSON(server.game)
		return &mcp.ReadResourceResult{
			Contents: []*mcp.ResourceContents{{
				URI:      req.Params.URI,
				MIMEType: "application/json",
				Text:     stateJSON,
			}},
		}, nil
	})

	mcpServer.AddResource(&mcp.Resource{
		URI:         "game://room",
		Name:        "Current Room",
		Description: "Description of the current room: name, description, items, exits.",
		MIMEType:    "text/plain",
	}, func(_ context.Context, req *mcp.ReadResourceRequest) (*mcp.ReadResourceResult, error) {
		server.mu.Lock()
		defer server.mu.Unlock()
		roomDesc := DescribeRoom(server.game)
		return &mcp.ReadResourceResult{
			Contents: []*mcp.ResourceContents{{
				URI:      req.Params.URI,
				MIMEType: "text/plain",
				Text:     roomDesc,
			}},
		}, nil
	})

	mcpServer.AddResource(&mcp.Resource{
		URI:         "game://inventory",
		Name:        "Inventory",
		Description: "List of items the player is carrying.",
		MIMEType:    "text/plain",
	}, func(_ context.Context, req *mcp.ReadResourceRequest) (*mcp.ReadResourceResult, error) {
		server.mu.Lock()
		defer server.mu.Unlock()
		invDesc := DescribeInventory(server.game)
		return &mcp.ReadResourceResult{
			Contents: []*mcp.ResourceContents{{
				URI:      req.Params.URI,
				MIMEType: "text/plain",
				Text:     invDesc,
			}},
		}, nil
	})

	// Register MCP Prompt
	mcpServer.AddPrompt(&mcp.Prompt{
		Name:        "play",
		Description: "System prompt for an LLM agent playing Dustwood. Provides game overview, commands, and strategy.",
	}, func(_ context.Context, _ *mcp.GetPromptRequest) (*mcp.GetPromptResult, error) {
		return &mcp.GetPromptResult{
			Description: "Dustwood game-playing instructions and strategy guide",
			Messages: []*mcp.PromptMessage{{
				Role: "user",
				Content: &mcp.TextContent{
					Text: dustwoodSystemPrompt,
				},
			}},
		}, nil
	})

	return mcpServer
}

func isAllowedOrigin(r *http.Request, allowed map[string]struct{}) bool {
	origin := r.Header.Get("Origin")
	if origin == "" {
		return true
	}
	_, ok := allowed[origin]
	return ok
}
