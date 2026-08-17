package main

import (
	"strings"
	"testing"
)

func TestExecuteCommandFinishedGameIsImmutable(t *testing.T) {
	seed := int64(43)
	game := NewGame(&seed, 30, nil)
	game.IsPlaying = false

	roomID := game.CurrentRoom.ID
	turns := game.Turns
	score := game.Score
	thirst := game.Thirst
	inventory := SummarizeState(game).Inventory

	output, summary := ExecuteCommand(game, "EAST")

	if !strings.Contains(output, "GAME OVER") {
		t.Fatalf("expected terminal output, got %q", output)
	}
	if summary.IsPlaying {
		t.Fatal("finished game became active")
	}
	if game.CurrentRoom.ID != roomID {
		t.Errorf("finished game moved from room %d to %d", roomID, game.CurrentRoom.ID)
	}
	if game.Turns != turns || game.Score != score || game.Thirst != thirst {
		t.Errorf("finished game state changed: before turns=%d score=%d thirst=%d, after turns=%d score=%d thirst=%d", turns, score, thirst, game.Turns, game.Score, game.Thirst)
	}
	if got := SummarizeState(game).Inventory; strings.Join(got, "\x00") != strings.Join(inventory, "\x00") {
		t.Errorf("finished game inventory changed: before=%v after=%v", inventory, got)
	}
}
