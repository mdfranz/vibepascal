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
	mu          sync.Mutex
	game        *GameState
	defaultSeed *int64
	turnLimit   int
}

func NewMCPServer(seed *int64, turnLimit int) *MCPServer {
	return &MCPServer{
		game:        NewGame(seed, turnLimit, io.Discard),
		defaultSeed: seed,
		turnLimit:   turnLimit,
	}
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
func (s *MCPServer) HandleLook(_ context.Context, _ *mcp.CallToolRequest, _ interface{}) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, "look")
	slog.Info("tool", "name", "look", "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "go" tool
func (s *MCPServer) HandleGo(_ context.Context, _ *mcp.CallToolRequest, input *GoInput) (*mcp.CallToolResult, *CommandOutput, error) {
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

	output, summary := ExecuteCommand(s.game, verb)
	slog.Info("tool", "name", "go", "direction", dir, "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "take" tool
func (s *MCPServer) HandleTake(_ context.Context, _ *mcp.CallToolRequest, input *TakeInput) (*mcp.CallToolResult, *CommandOutput, error) {
	if err := validateItemName(input.Item); err != nil {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{
			Output: err.Error(),
			State:  GameSummary{},
		}, nil
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, fmt.Sprintf("take %s", strings.TrimSpace(input.Item)))
	slog.Info("tool", "name", "take", "item", input.Item, "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "drop" tool
func (s *MCPServer) HandleDrop(_ context.Context, _ *mcp.CallToolRequest, input *DropInput) (*mcp.CallToolResult, *CommandOutput, error) {
	if err := validateItemName(input.Item); err != nil {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{
			Output: err.Error(),
			State:  GameSummary{},
		}, nil
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, fmt.Sprintf("drop %s", strings.TrimSpace(input.Item)))
	slog.Info("tool", "name", "drop", "item", input.Item, "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "inventory" tool
func (s *MCPServer) HandleInventory(_ context.Context, _ *mcp.CallToolRequest, _ interface{}) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, "inv")
	slog.Info("tool", "name", "inventory", "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "drink" tool
func (s *MCPServer) HandleDrink(_ context.Context, _ *mcp.CallToolRequest, _ interface{}) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, "drink")
	slog.Info("tool", "name", "drink", "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "water_horse" tool
func (s *MCPServer) HandleWaterHorse(_ context.Context, _ *mcp.CallToolRequest, _ interface{}) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, "water horse")
	slog.Info("tool", "name", "water_horse", "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "light" tool
func (s *MCPServer) HandleLight(_ context.Context, _ *mcp.CallToolRequest, _ interface{}) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, "light lamp")
	slog.Info("tool", "name", "light", "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "score" tool
func (s *MCPServer) HandleScore(_ context.Context, _ *mcp.CallToolRequest, _ interface{}) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	output, summary := ExecuteCommand(s.game, "score")
	slog.Info("tool", "name", "score", "room", summary.RoomName, "turn", summary.Turns)

	if !summary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: output, State: summary}, nil
	}

	return nil, &CommandOutput{Output: output, State: summary}, nil
}

// Handler for the "reset_game" tool
func (s *MCPServer) HandleResetGame(_ context.Context, _ *mcp.CallToolRequest, input *ResetGameInput) (*mcp.CallToolResult, *CommandOutput, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	seed := s.defaultSeed
	if input != nil && input.Seed != nil {
		seed = input.Seed
	}

	var buf bytes.Buffer
	s.game = NewGame(seed, s.turnLimit, &buf)
	resetSummary := SummarizeState(s.game)

	slog.Info("tool", "name", "reset_game", "room", resetSummary.RoomName, "turn", resetSummary.Turns)

	if !resetSummary.IsPlaying {
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{Output: buf.String(), State: resetSummary}, nil
	}

	return nil, &CommandOutput{Output: buf.String(), State: resetSummary}, nil
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

	if s.IsPlaying && s.TurnLimit > 0 && s.Turns >= s.TurnLimit {
		outPrintln(s)
		outPrintln(s, "⏳ You have taken too long. The sun dips below the horizon.")
		outPrintln(s, "GAME OVER.")
		s.IsPlaying = false
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
		s.game = NewGame(seed, s.turnLimit, &buf)
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
			result := &mcp.CallToolResult{IsError: true}
			return result, &CommandOutput{
				Output: buf.String(),
				State:  resetSummary,
			}, nil
		}

		return nil, &CommandOutput{
			Output: buf.String(),
			State:  resetSummary,
		}, nil
	}

	output, summary := ExecuteCommand(s.game, input.Command)
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
		result := &mcp.CallToolResult{IsError: true}
		return result, &CommandOutput{
			Output: output,
			State:  summary,
		}, nil
	}

	return nil, &CommandOutput{
		Output: output,
		State:  summary,
	}, nil
}

func RunMCPHTTP(server *MCPServer, addr, path string, origins []string, token string, jsonResponse bool, stateless bool) error {
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
	slog.Info("listening",
		"addr", addr,
		"path", path,
		"stateless", stateless,
		"json_response", jsonResponse,
	)
	return serverHTTP.ListenAndServe()
}

func RunMCPStdio(server *MCPServer) error {
	mcpServer := createMCPServer(server)
	return mcpServer.Run(context.Background(), &mcp.StdioTransport{})
}

func createMCPServer(server *MCPServer) *mcp.Server {
	mcpServer := mcp.NewServer(&mcp.Implementation{
		Name:    "dustwood-go",
		Version: "v1.0.0",
	}, nil)

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
