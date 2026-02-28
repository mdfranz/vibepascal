package main

import (
	"fmt"
	"math/rand"
	"os"
	"time"
)

func main() {
	var s GameState
	initState(&s)
	s.IsHeadless = len(os.Args) > 1 && os.Args[1] == "--headless"
	loadWorld(&s, "data/world.ini")
	rand.Seed(time.Now().UnixNano()) //nolint:staticcheck
	randomizeMapLocation(&s)
	s.CurrentRoom = s.RoomRegistry[1]
	s.IsPlaying = true
	look(&s)

	turnLimit := 50
	for s.IsPlaying {
		cmd := customReadLn(&s, "> ")
		processCommand(&s, cmd)
		if s.Turns >= turnLimit && s.IsPlaying {
			fmt.Println()
			fmt.Println("⏳ You have taken too long. The sun dips below the horizon.")
			fmt.Println("GAME OVER.")
			s.IsPlaying = false
		}
	}
	fmt.Println()
	fmt.Printf("🏆 Final score: %d\n", s.Score)
}
