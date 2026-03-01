package main

import (
	"fmt"
	"os"
	"strings"

	bolt "go.etcd.io/bbolt"
)

func boolStr(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

func saveGame(s *GameState, path string) {
	db, err := bolt.Open(path, 0600, nil)
	if err != nil {
		outPrintf(s, "Error saving game: %v\n", err)
		return
	}
	defer db.Close()

	err = db.Update(func(tx *bolt.Tx) error {
		// --- State bucket ---
		if err := tx.DeleteBucket([]byte("state")); err != nil && err != bolt.ErrBucketNotFound {
			return err
		}
		state, err := tx.CreateBucket([]byte("state"))
		if err != nil {
			return err
		}

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

		pairs := map[string]string{
			"CurrentRoom":      fmt.Sprintf("%d", s.CurrentRoom.ID),
			"IsPumpFixed":      boolStr(s.IsPumpFixed),
			"IsLampLit":        boolStr(s.IsLampLit),
			"HasWater":         boolStr(s.HasWater),
			"IsHorseSaddled":   boolStr(s.IsHorseSaddled),
			"IsRiding":         boolStr(s.IsRiding),
			"IsTelegraphFixed": boolStr(s.IsTelegraphFixed),
			"TempLightTurns":   fmt.Sprintf("%d", s.TempLightTurns),
			"CanteenDrinks":    fmt.Sprintf("%d", s.CanteenDrinks),
			"Thirst":           fmt.Sprintf("%d", s.Thirst),
			"HorseThirst":      fmt.Sprintf("%d", s.HorseThirst),
			"Turns":            fmt.Sprintf("%d", s.Turns),
			"Score":            fmt.Sprintf("%d", s.Score),
			"RoomBurning":      burningStr,
		}
		for k, v := range pairs {
			if err := state.Put([]byte(k), []byte(v)); err != nil {
				return err
			}
		}

		// --- Items bucket ---
		if err := tx.DeleteBucket([]byte("items")); err != nil && err != bolt.ErrBucketNotFound {
			return err
		}
		items, err := tx.CreateBucket([]byte("items"))
		if err != nil {
			return err
		}
		for i := 1; i <= MaxItems; i++ {
			sub, err := items.CreateBucket([]byte(fmt.Sprintf("Item%d", i)))
			if err != nil {
				return err
			}
			if err := sub.Put([]byte("Location"), []byte(fmt.Sprintf("%d", s.Items[i].Location))); err != nil {
				return err
			}
			if err := sub.Put([]byte("Description"), []byte(s.Items[i].Description)); err != nil {
				return err
			}
		}

		// --- ScoreFlags bucket ---
		if err := tx.DeleteBucket([]byte("scoreflags")); err != nil && err != bolt.ErrBucketNotFound {
			return err
		}
		flags, err := tx.CreateBucket([]byte("scoreflags"))
		if err != nil {
			return err
		}

		roomsStr := ""
		for i := 1; i <= MaxRooms; i++ {
			if s.RoomVisited[i] {
				roomsStr += "1"
			} else {
				roomsStr += "0"
			}
		}
		itemsStr := ""
		for i := 1; i <= MaxItems; i++ {
			if s.ItemScored[i] {
				itemsStr += "1"
			} else {
				itemsStr += "0"
			}
		}

		scoreFlags := map[string]string{
			"RoomVisited":        roomsStr,
			"ItemScored":         itemsStr,
			"ScoredPumpFix":      boolStr(s.ScoredPumpFix),
			"ScoredFirstFill":    boolStr(s.ScoredFirstFill),
			"ScoredLampLight":    boolStr(s.ScoredLampLight),
			"ScoredBoxOpen":      boolStr(s.ScoredBoxOpen),
			"ScoredTelegraphFix": boolStr(s.ScoredTelegraphFix),
			"ScoredOutlawKill":   boolStr(s.ScoredOutlawKill),
			"ScoredNoteFound":    boolStr(s.ScoredNoteFound),
		}
		for k, v := range scoreFlags {
			if err := flags.Put([]byte(k), []byte(v)); err != nil {
				return err
			}
		}

		return nil
	})

	if err != nil {
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

	db, err := bolt.Open(path, 0600, &bolt.Options{ReadOnly: true})
	if err != nil {
		outPrintf(s, "Error loading save file: %v\n", err)
		return
	}
	defer db.Close()

	err = db.View(func(tx *bolt.Tx) error {
		// --- State bucket ---
		state := tx.Bucket([]byte("state"))
		if state != nil {
			get := func(key string) string {
				v := state.Get([]byte(key))
				if v == nil {
					return ""
				}
				return string(v)
			}
			getInt := func(key string, def int) int {
				var n int
				if _, err := fmt.Sscanf(get(key), "%d", &n); err != nil {
					return def
				}
				return n
			}

			roomID := getInt("CurrentRoom", 1)
			if roomID >= 1 && roomID <= MaxRooms && s.RoomRegistry[roomID] != nil {
				s.CurrentRoom = s.RoomRegistry[roomID]
			}

			s.IsPumpFixed = parseBool(get("IsPumpFixed"))
			s.IsLampLit = parseBool(get("IsLampLit"))
			s.HasWater = parseBool(get("HasWater"))
			s.IsHorseSaddled = parseBool(get("IsHorseSaddled"))
			s.IsRiding = parseBool(get("IsRiding"))
			s.IsTelegraphFixed = parseBool(get("IsTelegraphFixed"))
			s.TempLightTurns = getInt("TempLightTurns", 0)
			s.CanteenDrinks = getInt("CanteenDrinks", 0)
			s.Thirst = getInt("Thirst", 0)
			s.HorseThirst = getInt("HorseThirst", 0)
			s.Turns = getInt("Turns", 0)
			s.Score = getInt("Score", 0)

			burningStr := get("RoomBurning")
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
		}

		// --- Items bucket ---
		items := tx.Bucket([]byte("items"))
		if items != nil {
			for i := 1; i <= MaxItems; i++ {
				sub := items.Bucket([]byte(fmt.Sprintf("Item%d", i)))
				if sub == nil {
					continue
				}
				if loc := sub.Get([]byte("Location")); loc != nil {
					var n int
					if _, err := fmt.Sscanf(string(loc), "%d", &n); err == nil {
						s.Items[i].Location = n
					}
				}
				if desc := sub.Get([]byte("Description")); desc != nil && len(desc) > 0 {
					s.Items[i].Description = string(desc)
				}
			}
		}

		// --- ScoreFlags bucket ---
		flags := tx.Bucket([]byte("scoreflags"))
		if flags != nil {
			get := func(key string) string {
				v := flags.Get([]byte(key))
				if v == nil {
					return ""
				}
				return string(v)
			}

			roomsStr := get("RoomVisited")
			for i := 1; i <= MaxRooms; i++ {
				if i <= len(roomsStr) {
					s.RoomVisited[i] = roomsStr[i-1] == '1'
				} else {
					s.RoomVisited[i] = false
				}
			}

			itemsStr := get("ItemScored")
			for i := 1; i <= MaxItems; i++ {
				if i <= len(itemsStr) {
					s.ItemScored[i] = itemsStr[i-1] == '1'
				} else {
					s.ItemScored[i] = false
				}
			}

			s.ScoredPumpFix = parseBool(get("ScoredPumpFix"))
			s.ScoredFirstFill = parseBool(get("ScoredFirstFill"))
			s.ScoredLampLight = parseBool(get("ScoredLampLight"))
			s.ScoredBoxOpen = parseBool(get("ScoredBoxOpen"))
			s.ScoredTelegraphFix = parseBool(get("ScoredTelegraphFix"))
			s.ScoredOutlawKill = parseBool(get("ScoredOutlawKill"))
			s.ScoredNoteFound = parseBool(get("ScoredNoteFound"))
		}

		return nil
	})

	if err != nil {
		outPrintf(s, "Error loading save file: %v\n", err)
		return
	}

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
