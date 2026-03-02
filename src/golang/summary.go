package main

import (
	"encoding/json"
	"fmt"
	"strings"
)

type GameSummary struct {
	RoomID      int      `json:"room_id" jsonschema:"Current room ID"`
	RoomName    string   `json:"room_name" jsonschema:"Current room name"`
	Turns       int      `json:"turns" jsonschema:"Number of turns taken"`
	Score       int      `json:"score" jsonschema:"Current score"`
	IsPlaying   bool     `json:"is_playing" jsonschema:"Whether the game is still active"`
	IsRiding    bool     `json:"is_riding" jsonschema:"Whether the player is riding"`
	IsDark      bool     `json:"is_dark" jsonschema:"Whether the room is dark"`
	Thirst      int      `json:"thirst" jsonschema:"Player thirst counter"`
	HorseThirst int      `json:"horse_thirst" jsonschema:"Horse thirst counter"`
	HasWater    bool     `json:"has_water" jsonschema:"Whether the canteen has water"`
	LampLit     bool     `json:"lamp_lit" jsonschema:"Whether the lamp is lit"`
	HorseSaddled bool    `json:"horse_saddled" jsonschema:"Whether the horse is saddled"`
	Inventory   []string `json:"inventory" jsonschema:"Inventory item descriptions"`
}

func SummarizeState(s *GameState) GameSummary {
	summary := GameSummary{
		RoomID:      0,
		RoomName:    "",
		Turns:       s.Turns,
		Score:       s.Score,
		IsPlaying:   s.IsPlaying,
		IsRiding:    s.IsRiding,
		IsDark:      isDark(s),
		Thirst:      s.Thirst,
		HorseThirst: s.HorseThirst,
		HasWater:    s.HasWater,
		LampLit:     s.IsLampLit,
		HorseSaddled: s.IsHorseSaddled,
	}
	if s.CurrentRoom != nil {
		summary.RoomID = s.CurrentRoom.ID
		summary.RoomName = s.CurrentRoom.Name
	}
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == InvLocation {
			summary.Inventory = append(summary.Inventory, s.Items[i].Description)
		}
	}
	return summary
}

// DescribeRoom returns a text description of the current room without mutating state.
func DescribeRoom(s *GameState) string {
	if s.CurrentRoom == nil {
		return "You are in an unknown location."
	}

	var desc strings.Builder
	desc.WriteString(fmt.Sprintf("%s\n", s.CurrentRoom.Name))
	desc.WriteString(fmt.Sprintf("%s\n", s.CurrentRoom.Description))

	// List visible items in room
	var items []string
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == s.CurrentRoom.ID {
			items = append(items, s.Items[i].Description)
		}
	}
	if len(items) > 0 {
		desc.WriteString("Items: " + strings.Join(items, ", ") + "\n")
	}

	// List visible exits
	var exits []string
	if s.CurrentRoom.North != nil {
		exits = append(exits, "north")
	}
	if s.CurrentRoom.South != nil {
		exits = append(exits, "south")
	}
	if s.CurrentRoom.East != nil {
		exits = append(exits, "east")
	}
	if s.CurrentRoom.West != nil {
		exits = append(exits, "west")
	}
	if len(exits) > 0 {
		desc.WriteString("Exits: " + strings.Join(exits, ", "))
	}

	return strings.TrimSpace(desc.String())
}

// DescribeInventory returns a text description of the player's inventory without mutating state.
func DescribeInventory(s *GameState) string {
	var items []string
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == InvLocation {
			items = append(items, s.Items[i].Description)
		}
	}

	if len(items) == 0 {
		return "You are carrying nothing."
	}

	return "You are carrying: " + strings.Join(items, ", ")
}

// SummarizeStateJSON returns the game state as a JSON string.
func SummarizeStateJSON(s *GameState) string {
	summary := SummarizeState(s)
	data, err := json.Marshal(summary)
	if err != nil {
		return "{}"
	}
	return string(data)
}
