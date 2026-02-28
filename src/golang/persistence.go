package main

import (
	"fmt"
	"os"
	"strings"

	"gopkg.in/ini.v1"
)

func boolStr(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

func saveGame(s *GameState, path string) {
	cfg := ini.Empty()

	sec, _ := cfg.NewSection("State")
	sec.Key("CurrentRoom").SetValue(fmt.Sprintf("%d", s.CurrentRoom.ID))
	sec.Key("IsPumpFixed").SetValue(boolStr(s.IsPumpFixed))
	sec.Key("IsLampLit").SetValue(boolStr(s.IsLampLit))
	sec.Key("HasWater").SetValue(boolStr(s.HasWater))
	sec.Key("IsHorseSaddled").SetValue(boolStr(s.IsHorseSaddled))
	sec.Key("IsRiding").SetValue(boolStr(s.IsRiding))
	sec.Key("IsTelegraphFixed").SetValue(boolStr(s.IsTelegraphFixed))
	sec.Key("TempLightTurns").SetValue(fmt.Sprintf("%d", s.TempLightTurns))
	sec.Key("CanteenDrinks").SetValue(fmt.Sprintf("%d", s.CanteenDrinks))
	sec.Key("Thirst").SetValue(fmt.Sprintf("%d", s.Thirst))
	sec.Key("HorseThirst").SetValue(fmt.Sprintf("%d", s.HorseThirst))
	sec.Key("Turns").SetValue(fmt.Sprintf("%d", s.Turns))
	sec.Key("Score").SetValue(fmt.Sprintf("%d", s.Score))

	// RoomBurning as digit string
	burningStr := ""
	for i := 1; i <= MaxRooms; i++ {
		v := s.RoomBurning[i]
		if v < 0 {
			v = 9
		} else if v > 9 {
			v = 9
		}
		burningStr += fmt.Sprintf("%d", v)
	}
	sec.Key("RoomBurning").SetValue(burningStr)

	for i := 1; i <= MaxItems; i++ {
		sectionName := fmt.Sprintf("Item%d", i)
		itemSec, _ := cfg.NewSection(sectionName)
		itemSec.Key("Location").SetValue(fmt.Sprintf("%d", s.Items[i].Location))
		itemSec.Key("Description").SetValue(s.Items[i].Description)
	}

	// ScoreFlags section
	flagSec, _ := cfg.NewSection("ScoreFlags")

	roomsStr := ""
	for i := 1; i <= MaxRooms; i++ {
		if s.RoomVisited[i] {
			roomsStr += "1"
		} else {
			roomsStr += "0"
		}
	}
	flagSec.Key("RoomVisited").SetValue(roomsStr)

	itemsStr := ""
	for i := 1; i <= MaxItems; i++ {
		if s.ItemScored[i] {
			itemsStr += "1"
		} else {
			itemsStr += "0"
		}
	}
	flagSec.Key("ItemScored").SetValue(itemsStr)
	flagSec.Key("ScoredPumpFix").SetValue(boolStr(s.ScoredPumpFix))
	flagSec.Key("ScoredFirstFill").SetValue(boolStr(s.ScoredFirstFill))
	flagSec.Key("ScoredLampLight").SetValue(boolStr(s.ScoredLampLight))
	flagSec.Key("ScoredBoxOpen").SetValue(boolStr(s.ScoredBoxOpen))
	flagSec.Key("ScoredTelegraphFix").SetValue(boolStr(s.ScoredTelegraphFix))
	flagSec.Key("ScoredOutlawKill").SetValue(boolStr(s.ScoredOutlawKill))
	flagSec.Key("ScoredNoteFound").SetValue(boolStr(s.ScoredNoteFound))

	if err := cfg.SaveTo(path); err != nil {
		outPrintf(s, "Error saving game: %v\n", err)
		return
	}
	outPrintln(s, "💾 Game saved.")
}

func loadGame(s *GameState, path string) {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		outPrintln(s, "No save file found.")
		return
	}

	cfg, err := ini.Load(path)
	if err != nil {
		outPrintf(s, "Error loading save file: %v\n", err)
		return
	}

	stateSec := cfg.Section("State")
	roomID := stateSec.Key("CurrentRoom").MustInt(1)
	if roomID >= 1 && roomID <= MaxRooms && s.RoomRegistry[roomID] != nil {
		s.CurrentRoom = s.RoomRegistry[roomID]
	}

	s.IsPumpFixed = parseBool(stateSec.Key("IsPumpFixed").String())
	s.IsLampLit = parseBool(stateSec.Key("IsLampLit").String())
	s.HasWater = parseBool(stateSec.Key("HasWater").String())
	s.IsHorseSaddled = parseBool(stateSec.Key("IsHorseSaddled").String())
	s.IsRiding = parseBool(stateSec.Key("IsRiding").String())
	s.IsTelegraphFixed = parseBool(stateSec.Key("IsTelegraphFixed").String())
	s.TempLightTurns = stateSec.Key("TempLightTurns").MustInt(0)
	s.CanteenDrinks = stateSec.Key("CanteenDrinks").MustInt(0)
	s.Thirst = stateSec.Key("Thirst").MustInt(0)
	s.HorseThirst = stateSec.Key("HorseThirst").MustInt(0)
	s.Turns = stateSec.Key("Turns").MustInt(0)
	s.Score = stateSec.Key("Score").MustInt(0)

	burningStr := stateSec.Key("RoomBurning").String()
	for i := 1; i <= MaxRooms; i++ {
		if i <= len(burningStr) {
			ch := burningStr[i-1]
			if ch >= '0' && ch <= '9' {
				s.RoomBurning[i] = int(ch - '0')
			} else {
				s.RoomBurning[i] = 0
			}
		} else {
			s.RoomBurning[i] = 0
		}
	}

	for i := 1; i <= MaxItems; i++ {
		sectionName := fmt.Sprintf("Item%d", i)
		if cfg.HasSection(sectionName) {
			itemSec := cfg.Section(sectionName)
			s.Items[i].Location = itemSec.Key("Location").MustInt(s.Items[i].Location)
			desc := itemSec.Key("Description").String()
			if desc != "" {
				s.Items[i].Description = desc
			}
		}
	}

	flagSec := cfg.Section("ScoreFlags")
	roomsStr := flagSec.Key("RoomVisited").String()
	for i := 1; i <= MaxRooms; i++ {
		if i <= len(roomsStr) {
			s.RoomVisited[i] = roomsStr[i-1] == '1'
		} else {
			s.RoomVisited[i] = false
		}
	}

	itemsStr := flagSec.Key("ItemScored").String()
	for i := 1; i <= MaxItems; i++ {
		if i <= len(itemsStr) {
			s.ItemScored[i] = itemsStr[i-1] == '1'
		} else {
			s.ItemScored[i] = false
		}
	}

	s.ScoredPumpFix = parseBool(flagSec.Key("ScoredPumpFix").String())
	s.ScoredFirstFill = parseBool(flagSec.Key("ScoredFirstFill").String())
	s.ScoredLampLight = parseBool(flagSec.Key("ScoredLampLight").String())
	s.ScoredBoxOpen = parseBool(flagSec.Key("ScoredBoxOpen").String())
	s.ScoredTelegraphFix = parseBool(flagSec.Key("ScoredTelegraphFix").String())
	s.ScoredOutlawKill = parseBool(flagSec.Key("ScoredOutlawKill").String())
	s.ScoredNoteFound = parseBool(flagSec.Key("ScoredNoteFound").String())

	if s.IsTelegraphFixed && s.RoomRegistry[2] != nil {
		s.RoomRegistry[2].Description = "The telegraph has been repaired. The line hums faintly with life."
	}

	outPrintln(s, "📂 Game loaded.")
	look(s)
}

// parseBool handles true/false, -1/0, 1/0 from Pascal TIniFile
func parseBool(v string) bool {
	switch strings.ToLower(strings.TrimSpace(v)) {
	case "true", "1", "-1", "yes":
		return true
	}
	return false
}
