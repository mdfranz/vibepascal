package main

import (
	"io"
	"math/rand"
	"time"
)

func NewGame(seed *int64, out io.Writer) *GameState {
	var s GameState
	initState(&s)
	if out != nil {
		s.Out = out
	}
	loadWorld(&s, "data/world.ini")
	if seed != nil {
		rand.Seed(*seed) //nolint:staticcheck
	} else {
		rand.Seed(time.Now().UnixNano()) //nolint:staticcheck
	}
	randomizeMapLocation(&s)
	s.CurrentRoom = s.RoomRegistry[1]
	s.IsPlaying = true
	look(&s)
	return &s
}
