package main

import (
	"fmt"
	"math/rand"
	"strings"

	"gopkg.in/ini.v1"
)

func loadWorld(s *GameState, path string) {
	cfg, err := ini.Load(path)
	if err != nil {
		fmt.Printf("Error loading world: %v\n", err)
		return
	}

	// First pass: create rooms
	for i := 1; i <= MaxRooms; i++ {
		sectionName := fmt.Sprintf("Room%d", i)
		if !cfg.HasSection(sectionName) {
			continue
		}
		sec := cfg.Section(sectionName)
		r := &Room{
			ID:          i,
			Name:        sec.Key("Name").String(),
			Description: sec.Key("Description").String(),
		}
		s.RoomRegistry[i] = r
	}

	// Second pass: link exits
	for i := 1; i <= MaxRooms; i++ {
		if s.RoomRegistry[i] == nil {
			continue
		}
		sectionName := fmt.Sprintf("Room%d", i)
		sec := cfg.Section(sectionName)

		n := sec.Key("North").MustInt(0)
		so := sec.Key("South").MustInt(0)
		e := sec.Key("East").MustInt(0)
		w := sec.Key("West").MustInt(0)

		if n > 0 && n <= MaxRooms {
			s.RoomRegistry[i].North = s.RoomRegistry[n]
		}
		if so > 0 && so <= MaxRooms {
			s.RoomRegistry[i].South = s.RoomRegistry[so]
		}
		if e > 0 && e <= MaxRooms {
			s.RoomRegistry[i].East = s.RoomRegistry[e]
		}
		if w > 0 && w <= MaxRooms {
			s.RoomRegistry[i].West = s.RoomRegistry[w]
		}
	}

	// Load items
	for i := 1; i <= MaxItems; i++ {
		sectionName := fmt.Sprintf("Item%d", i)
		if !cfg.HasSection(sectionName) {
			continue
		}
		sec := cfg.Section(sectionName)
		s.Items[i].Name = strings.ToUpper(sec.Key("Name").String())
		s.Items[i].Description = sec.Key("Description").String()
		s.Items[i].Details = sec.Key("Details").String()
		s.Items[i].Location = sec.Key("Location").MustInt(0)
		s.Items[i].IsTakeable = sec.Key("IsTakeable").MustInt(0) == 1
	}
}

func randomizeMapLocation(s *GameState) {
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Name == "MAP" {
			s.Items[i].Location = rand.Intn(7) + 1
			break
		}
	}
}
