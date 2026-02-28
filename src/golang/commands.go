package main

import (
	"math/rand"
	"strings"
)

type commandHandler func(s *GameState, noun string, consumeTurn *bool)

type commandEntry struct {
	verb    string
	handler commandHandler
}

func splitCommand(cmd string) (verb, noun string) {
	trimmed := strings.TrimSpace(cmd)
	spacePos := strings.Index(trimmed, " ")
	if spacePos >= 0 {
		verb = strings.ToUpper(trimmed[:spacePos])
		noun = strings.TrimSpace(trimmed[spacePos+1:])
	} else {
		verb = strings.ToUpper(trimmed)
		noun = ""
	}
	return
}

func findItem(name string, loc int, s *GameState) int {
	upper := strings.ToUpper(name)
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == loc && s.Items[i].Name == upper {
			return i
		}
	}
	return 0
}

func findItemAny(name string, s *GameState) int {
	upper := strings.ToUpper(name)
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Name == upper {
			return i
		}
	}
	return 0
}

func isDesertRoom(id int) bool {
	return id >= 8 && id <= 13
}

func printMovement(s *GameState, direction string, isRiding bool) {
	idx := rand.Intn(5)
	var emoji, msg string
	if isRiding {
		emoji = "🏇 "
		switch idx {
		case 0:
			msg = "You ride " + direction + "."
		case 1:
			msg = "You guide your horse " + direction + "."
		case 2:
			msg = "You trot " + direction + "."
		case 3:
			msg = "You and your mare head " + direction + "."
		case 4:
			msg = "The horse carries you " + direction + "."
		}
	} else {
		emoji = "🚶 "
		switch idx {
		case 0:
			msg = "You walk " + direction + "."
		case 1:
			msg = "You trek " + direction + " through the dust."
		case 2:
			msg = "You head " + direction + "."
		case 3:
			msg = "You make your way " + direction + "."
		case 4:
			msg = "You trudge " + direction + " across the dry ground."
		}
	}
	outPrintln(s, emoji+msg)
}

func moveTo(s *GameState, newRoom *Room) {
	if newRoom == nil {
		outPrintln(s, "You cannot go that way.")
	} else if s.IsRiding && (newRoom.ID == 2 || newRoom.ID == 4 || newRoom.ID == 5 || newRoom.ID == 7) {
		outPrintln(s, "You can't bring a horse in there. Dismount first.")
	} else if s.CurrentRoom.ID == 6 && newRoom.ID == DesertEntryRoomID && !s.IsRiding {
		outPrintln(s, "The desert is too dangerous on foot. You must be riding a saddled horse.")
	} else {
		s.CurrentRoom = newRoom
		if s.IsRiding {
			for i := 1; i <= MaxItems; i++ {
				if s.Items[i].Name == "HORSE" {
					s.Items[i].Location = InvLocation
					break
				}
			}
		}
		if s.CurrentRoom.ID != 1 && !s.RoomVisited[s.CurrentRoom.ID] {
			s.RoomVisited[s.CurrentRoom.ID] = true
			s.Score += ScoreRoomVisit
		}
		if s.CurrentRoom.ID != 1 && rand.Intn(100) < 20 {
			s.SnakeRoom = s.CurrentRoom.ID
		} else {
			s.SnakeRoom = 0
		}
		if s.CurrentRoom.ID != 1 && s.CurrentRoom.ID != 7 && rand.Intn(100) < 15 {
			s.OutlawRoom = s.CurrentRoom.ID
		} else {
			s.OutlawRoom = 0
		}
		look(s)
	}
}

func updateWorld(s *GameState) {
	s.Turns++
	s.Thirst++
	if s.TempLightTurns > 0 && !s.IsLampLit {
		s.TempLightTurns--
	}
	if s.IsHorseSaddled && isDesertRoom(s.CurrentRoom.ID) {
		s.HorseThirst++
	}

	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location > 0 {
			burning := s.RoomBurning[s.Items[i].Location]
			if burning == 0 {
				continue
			} else if burning > 1 {
				continue
			} else {
				s.Items[i].Location = 0
				outPrintf(s, "🔥 The fire destroys %s.\n", s.Items[i].Description)
			}
		}
	}

	for i := 1; i <= MaxRooms; i++ {
		if s.RoomBurning[i] > 0 {
			s.RoomBurning[i]--
		}
	}

	if s.SnakeRoom > 0 && s.RoomBurning[s.SnakeRoom] > 0 {
		s.SnakeRoom = 0
		outPrintln(s, "🔥 The fire drives away the rattlesnake.")
	}
	if s.SnakeRoom > 0 && rand.Intn(100) < 30 {
		s.SnakeRoom = 0
	}

	if s.Thirst > ThirstLimit-5 {
		outPrintln(s)
		outPrintln(s, "🌵 === Your throat is parched. You need water soon. ===")
	}
	if s.IsHorseSaddled && isDesertRoom(s.CurrentRoom.ID) && s.HorseThirst > HorseThirstLimit-5 {
		outPrintln(s)
		outPrintln(s, "🐎 === Your horse is showing signs of exhaustion. It needs water soon. ===")
	}

	if s.Thirst >= ThirstLimit {
		outPrintln(s)
		wrapWriteLn(s, "💀 You have collapsed from dehydration. GAME OVER.")
		s.IsPlaying = false
	}

	if isDesertRoom(s.CurrentRoom.ID) && !s.IsRiding {
		outPrintln(s)
		wrapWriteLn(s, "🔥 The desert heat is overwhelming on foot. You collapse into the sand. GAME OVER.")
		s.IsPlaying = false
	}

	if s.IsHorseSaddled && isDesertRoom(s.CurrentRoom.ID) && s.HorseThirst >= HorseThirstLimit {
		outPrintln(s)
		wrapWriteLn(s, "💀 Your horse collapses from dehydration. You are stranded in the desert. GAME OVER.")
		s.IsPlaying = false
	}

	if s.Turns == TwilightTurn {
		outPrintln(s, "🌇 The sun is getting low.")
	}
	if s.Turns == DarkTurn {
		outPrintln(s, "🌑 It is now dark.")
	}
}

func cmdDrink(s *GameState, noun string, consumeTurn *bool) {
	if findItem("CANTEEN", InvLocation, s) == 0 {
		outPrintln(s, "You don't have anything to drink from.")
	} else if !s.HasWater {
		outPrintln(s, "Your canteen is empty.")
	} else {
		s.Thirst = 0
		if s.CanteenDrinks > 0 {
			s.CanteenDrinks--
		}
		if s.CanteenDrinks <= 0 {
			s.HasWater = false
		}
		wrapWriteLn(s, "💧 The water is warm but refreshing.")
		outPrintln(s, "Your thirst is quenched.")
	}
}

func cmdFillCanteen(s *GameState, noun string, consumeTurn *bool) {
	if findItem("CANTEEN", InvLocation, s) == 0 {
		outPrintln(s, "You have nothing to fill.")
	} else if s.CurrentRoom.ID == 3 && s.IsPumpFixed {
		s.HasWater = true
		s.CanteenDrinks = 3
		outPrintln(s, "💧 You fill your canteen with fresh water from the pump.")
		if !s.ScoredFirstFill {
			s.ScoredFirstFill = true
			s.Score += ScoreFirstFill
		}
	} else if s.CurrentRoom.ID == StreamRoomID {
		s.HasWater = true
		s.CanteenDrinks = 3
		outPrintln(s, "💧 You fill your canteen with cold stream water.")
	} else {
		outPrintln(s, "There is no water here.")
	}
}

func cmdLightLamp(s *GameState, noun string, consumeTurn *bool) {
	nounUpper := strings.ToUpper(strings.TrimSpace(noun))
	if noun != "" && nounUpper != "MATCH" && nounUpper != "MATCHES" {
		outPrintln(s, "Light what?")
		return
	}
	if findItem("LAMP", InvLocation, s) > 0 {
		s.IsLampLit = true
		wrapWriteLn(s, "🔦 You light the lamp. A yellow glow illuminates the room.")
		if !s.ScoredLampLight {
			s.ScoredLampLight = true
			s.Score += ScoreLampLight
		}
	} else {
		s.TempLightTurns = 3
		wrapWriteLn(s, "🔥 You strike a match. The room brightens for a moment.")
	}
}

func cmdShowHelp(s *GameState, noun string, consumeTurn *bool) {
	outPrintln(s)
	outPrintln(s, "Available Commands:")
	outPrintln(s, "  🚶 N, S, E, W      - Move North, South, East, West")
	outPrintln(s, "  👀 LOOK (L)        - Look around")
	outPrintln(s, "  🔍 EXAMINE (X)     - Look closely at an item")
	outPrintln(s, "  🖐️  TAKE (GET)      - Pick up an item")
	outPrintln(s, "  ✋  DROP            - Leave an item")
	outPrintln(s, "  🎒 INVENTORY (I)   - Check your gear")
	outPrintln(s, "  💧 DRINK           - Drink from your canteen")
	outPrintln(s, "  🚰 FILL            - Fill canteen at a water source")
	outPrintln(s, "  🐎 WATER           - Water your horse at a water source")
	outPrintln(s, "  🔦 LIGHT           - Light your lamp if you have matches")
	outPrintln(s, "  🔧 FIX             - Repair something")
	outPrintln(s, "  🏇 SADDLE          - Put a saddle on the horse")
	outPrintln(s, "  ❄️  FREEZE (WAIT)   - Stay still to avoid danger")
	outPrintln(s, "  🔥 BURN            - Burn a flammable item (requires matches)")
	outPrintln(s, "  🔥 FIRE            - Start a fire in certain rooms (requires matches)")
	outPrintln(s, "  🧗 CLIMB           - Climb a steep obstacle")
	outPrintln(s, "  💾 SAVE / LOAD     - Save or load your progress")
	outPrintln(s, "  🏆 SCORE           - Show current score")
	outPrintln(s, "  ❓ HELP (H)        - Show this list")
	outPrintln(s, "  🚪 QUIT (Q)        - Exit")
	outPrintln(s)
	*consumeTurn = false
}

func cmdExamineItem(s *GameState, targetNoun string, consumeTurn *bool) {
	noun := targetNoun
	if strings.ToUpper(noun[:min(3, len(noun))]) == "AT " {
		noun = strings.TrimSpace(noun[3:])
	}
	itemID := findItem(noun, InvLocation, s)
	if itemID == 0 {
		itemID = findItem(noun, s.CurrentRoom.ID, s)
	}
	if itemID > 0 {
		wrapWriteLn(s, s.Items[itemID].Details)
		if s.Items[itemID].Name == "ROCK" {
			keyId := findItemAny("KEY", s)
			if keyId > 0 && s.Items[keyId].Location == 0 {
				s.Items[keyId].Location = s.CurrentRoom.ID
				outPrintln(s)
				outPrintln(s, "You lift the rock. A small brass key is hidden beneath it.")
			}
		}
		if s.Items[itemID].Name == "BOOK" {
			noteId := findItemAny("NOTE", s)
			if noteId > 0 && s.Items[noteId].Location == 0 {
				s.Items[noteId].Location = InvLocation
				outPrintln(s)
				outPrintln(s, "A small folded note falls out of the book.")
				if !s.ScoredNoteFound {
					s.ScoredNoteFound = true
					s.Score += ScoreNoteFound
				}
			}
		}
	} else if noun == "" {
		look(s)
	} else {
		outPrintln(s, "You don't see that here.")
	}
	*consumeTurn = false
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func cmdFixSomething(s *GameState, targetNoun string, consumeTurn *bool) {
	noun := strings.ToUpper(strings.TrimSpace(targetNoun))
	if noun == "PUMP" && s.CurrentRoom.ID == 3 {
		if findItem("LEATHER", InvLocation, s) > 0 {
			s.IsPumpFixed = true
			outPrintln(s, "You fix the pump. Water starts to flow.")
			s.Items[3].Description = "a working water pump"
			if !s.ScoredPumpFix {
				s.ScoredPumpFix = true
				s.Score += ScorePumpFix
			}
		} else {
			outPrintln(s, "You need leather.")
		}
	} else if (noun == "WIRE" || noun == "WIRES" || noun == "TELEGRAPH") && s.CurrentRoom.ID == 2 {
		if s.IsTelegraphFixed {
			outPrintln(s, "The telegraph is already repaired.")
		} else if findItem("WIRE", InvLocation, s) > 0 {
			s.IsTelegraphFixed = true
			outPrintln(s, "You splice the copper wire and restore the telegraph line.")
			if s.RoomRegistry[2] != nil {
				s.RoomRegistry[2].Description = "The telegraph has been repaired. The line hums faintly with life."
			}
			if !s.ScoredTelegraphFix {
				s.ScoredTelegraphFix = true
				s.Score += ScoreTelegraphFix
			}
			s.Items[4].Location = 0
		} else {
			outPrintln(s, "You need copper wire.")
		}
	} else {
		outPrintln(s, "Nothing to fix here.")
	}
}

func cmdWaterHorse(s *GameState, targetNoun string, consumeTurn *bool) {
	noun := strings.ToUpper(strings.TrimSpace(targetNoun))
	if noun != "" && noun != "HORSE" && noun != "MARE" {
		outPrintln(s, "Water what?")
		return
	}
	if !s.IsHorseSaddled {
		outPrintln(s, "You don't have a horse with you.")
		return
	}
	if s.CurrentRoom.ID != StreamRoomID {
		outPrintln(s, "There is no water here for your horse.")
		return
	}
	s.HorseThirst = 0
	outPrintln(s, "Your horse drinks deeply from the stream.")
}

func cmdSaddleHorse(s *GameState, targetNoun string, consumeTurn *bool) {
	noun := strings.ToUpper(strings.TrimSpace(targetNoun))
	if noun != "" && noun != "HORSE" && noun != "ON HORSE" && noun != "MARE" {
		outPrintln(s, "Saddle what?")
		return
	}
	horseID := findItem("HORSE", s.CurrentRoom.ID, s)
	if horseID == 0 {
		outPrintln(s, "There is no horse here.")
		return
	}
	saddleID := findItem("SADDLE", InvLocation, s)
	if saddleID == 0 {
		outPrintln(s, "You need a saddle.")
		return
	}
	if s.IsHorseSaddled {
		outPrintln(s, "The horse is already saddled.")
		return
	}
	s.IsHorseSaddled = true
	s.Items[saddleID].Location = 0
	s.Items[horseID].Description = "a saddled horse"
	s.Items[horseID].Details = "A calm, saddle-ready horse. It looks steady and patient."
	outPrintln(s, "You secure the saddle onto the horse. It stands quietly.")
}

func cmdHandleMount(s *GameState, noun string, consumeTurn *bool) {
	nounUpper := strings.ToUpper(strings.TrimSpace(noun))
	if nounUpper != "" && nounUpper != "HORSE" && nounUpper != "MARE" {
		outPrintln(s, "Mount what?")
		return
	}
	if s.IsRiding {
		outPrintln(s, "You are already riding.")
	} else if findItem("HORSE", s.CurrentRoom.ID, s) > 0 {
		if s.IsHorseSaddled {
			s.IsRiding = true
			horseID := findItem("HORSE", s.CurrentRoom.ID, s)
			s.Items[horseID].Location = InvLocation
			outPrintln(s, "You swing yourself into the saddle. You are now riding.")
		} else {
			outPrintln(s, "The horse needs a saddle before you can ride her.")
		}
	} else {
		outPrintln(s, "There is no horse here.")
	}
}

func cmdHandleDismount(s *GameState, noun string, consumeTurn *bool) {
	nounUpper := strings.ToUpper(strings.TrimSpace(noun))
	if nounUpper != "" && nounUpper != "HORSE" && nounUpper != "MARE" {
		outPrintln(s, "Dismount what?")
		return
	}
	if !s.IsRiding {
		outPrintln(s, "You aren't riding anything.")
	} else {
		s.IsRiding = false
		horseID := findItem("HORSE", InvLocation, s)
		if horseID > 0 {
			s.Items[horseID].Location = s.CurrentRoom.ID
		}
		outPrintln(s, "You dismount and stand beside your horse.")
	}
}

func cmdHandleOpen(s *GameState, noun string, consumeTurn *bool) {
	if noun == "BOX" && s.CurrentRoom.ID == 7 {
		if s.IsBoxOpen {
			outPrintln(s, "It is already open.")
		} else if findItem("KEY", InvLocation, s) == 0 {
			outPrintln(s, "The box is locked. You need a key.")
		} else {
			s.IsBoxOpen = true
			s.Items[8].Location = 7
			outPrintln(s, "You unlock the box. Inside lies a heavy revolver.")
			if !s.ScoredBoxOpen {
				s.ScoredBoxOpen = true
				s.Score += ScoreBoxOpen
			}
		}
	} else {
		outPrintln(s, "There is nothing to open here.")
		*consumeTurn = false
	}
}

func cmdHandleShoot(s *GameState, noun string, consumeTurn *bool) {
	if findItem("REVOLVER", InvLocation, s) == 0 {
		outPrintln(s, "You have nothing to shoot with.")
	} else if s.OutlawRoom == s.CurrentRoom.ID {
		s.OutlawRoom = 0
		wrapWriteLn(s, "💥 You draw your revolver and fire first. The outlaw falls to the ground.")
		outPrintln(s, "💀 The threat is gone.")
		if !s.ScoredOutlawKill {
			s.ScoredOutlawKill = true
			s.Score += ScoreOutlawKill
		}
	} else {
		outPrintln(s, "Nothing here to shoot.")
	}
}

func cmdHandleFreeze(s *GameState, noun string, consumeTurn *bool) {
	outPrintln(s, "You stay perfectly still. The snake watches you...")
	if rand.Intn(100) < 50 {
		s.SnakeRoom = 0
		outPrintln(s, "The snake loses interest and slithers into the shadows.")
	}
}

func cmdHandleLook(s *GameState, noun string, consumeTurn *bool) {
	cmdExamineItem(s, noun, consumeTurn)
	*consumeTurn = false
}

func cmdHandleSearch(s *GameState, noun string, consumeTurn *bool) {
	look(s)
	*consumeTurn = false
}

func cmdHandleInventory(s *GameState, noun string, consumeTurn *bool) {
	outPrintln(s, "You are carrying:")
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == InvLocation {
			outPrintf(s, "  - %s\n", s.Items[i].Description)
		}
	}
	*consumeTurn = false
}

func cmdHandleScore(s *GameState, noun string, consumeTurn *bool) {
	outPrintf(s, "🏆 Score: %d\n", s.Score)
	*consumeTurn = false
}

func cmdHandleSave(s *GameState, noun string, consumeTurn *bool) {
	saveGame(s, "data/save.ini")
	*consumeTurn = false
}

func cmdHandleLoad(s *GameState, noun string, consumeTurn *bool) {
	loadGame(s, "data/save.ini")
	*consumeTurn = false
}

func cmdHandleTake(s *GameState, noun string, consumeTurn *bool) {
	itemID := findItem(noun, s.CurrentRoom.ID, s)
	if itemID > 0 {
		carryCount := 0
		for i := 1; i <= MaxItems; i++ {
			if s.Items[i].Location == InvLocation && s.Items[i].IsTakeable {
				carryCount++
			}
		}
		if carryCount >= MaxCarry {
			outPrintln(s, "You can't carry any more. Drop something first.")
			return
		}
		if !s.Items[itemID].IsTakeable {
			switch s.Items[itemID].Name {
			case "PUMP":
				outPrintln(s, "The pump is fixed in place.")
			case "HORSE":
				outPrintln(s, "It's too big to carry.")
			case "BOX":
				outPrintln(s, "It's bolted down.")
			case "ROCK":
				outPrintln(s, "It's too heavy to carry.")
			default:
				outPrintln(s, "You can't take that.")
			}
			return
		}
		s.Items[itemID].Location = InvLocation
		outPrintf(s, "🎒 Taken: %s.\n", s.Items[itemID].Description)
		if !s.ItemScored[itemID] {
			s.ItemScored[itemID] = true
			s.Score += ScoreItemPickup
		}
	} else {
		outPrintln(s, "Not here.")
	}
}

func cmdHandleQuit(s *GameState, noun string, consumeTurn *bool) {
	s.IsPlaying = false
}

func cmdHandleClimb(s *GameState, noun string, consumeTurn *bool) {
	if s.CurrentRoom.ID == 12 {
		moveTo(s, s.RoomRegistry[StreamRoomID])
	} else {
		outPrintln(s, "There is nothing to climb here.")
	}
}

func cmdHandlePut(s *GameState, noun string, consumeTurn *bool) {
	if strings.Contains(strings.ToUpper(noun), "SADDLE") {
		cmdSaddleHorse(s, "HORSE", consumeTurn)
	}
}

func cmdHandleDrop(s *GameState, noun string, consumeTurn *bool) {
	itemID := findItem(noun, InvLocation, s)
	if itemID > 0 {
		s.Items[itemID].Location = s.CurrentRoom.ID
		outPrintf(s, "✋ Dropped: %s.\n", s.Items[itemID].Description)
	} else {
		outPrintln(s, "You aren't carrying that.")
	}
}

func cmdHandleBurn(s *GameState, noun string, consumeTurn *bool) {
	target := strings.ToUpper(strings.TrimSpace(noun))
	if target == "" {
		outPrintln(s, "Burn what?")
		return
	}
	if findItem("MATCHES", InvLocation, s) == 0 {
		outPrintln(s, "You have nothing to burn it with.")
		return
	}
	itemID := findItem(target, InvLocation, s)
	if itemID == 0 {
		itemID = findItem(target, s.CurrentRoom.ID, s)
	}
	if itemID == 0 {
		outPrintln(s, "You don't see that here.")
		return
	}
	name := s.Items[itemID].Name
	if name != "BOOK" && name != "LEDGER" && name != "LEATHER" && name != "MAP" && name != "SADDLE" {
		outPrintln(s, "It doesn't burn.")
		return
	}
	s.Items[itemID].Location = 0
	outPrintln(s, "You burn it to ash.")
}

func cmdHandleFire(s *GameState, noun string, consumeTurn *bool) {
	if findItem("MATCHES", InvLocation, s) == 0 {
		outPrintln(s, "You have nothing to start a fire with.")
		return
	}
	id := s.CurrentRoom.ID
	if id != 2 && id != 3 && id != 5 {
		outPrintln(s, "There is nothing here that will catch fire.")
		return
	}
	if s.RoomBurning[id] > 0 {
		outPrintln(s, "A fire is already burning here.")
		return
	}
	s.RoomBurning[id] = 3
	outPrintln(s, "🔥 You start a fire. The room glows with heat.")
	if s.SnakeRoom == id {
		s.SnakeRoom = 0
		outPrintln(s, "🔥 The rattlesnake recoils from the flames and disappears.")
	}
}

var safeVerbs = map[string]bool{
	"N": true, "S": true, "E": true, "W": true,
	"NORTH": true, "SOUTH": true, "EAST": true, "WEST": true,
	"LOOK": true, "L": true, "EXAMINE": true, "X": true,
	"SEARCH": true, "INVENTORY": true, "I": true,
	"CHECK": true, "HELP": true, "?": true, "H": true,
	"SCORE": true, "SAVE": true, "LOAD": true,
	"QUIT": true, "Q": true,
}

func checkHazards(s *GameState, verb string) bool {
	if s.SnakeRoom == s.CurrentRoom.ID && !safeVerbs[verb] && verb != "FREEZE" && verb != "WAIT" {
		outPrintln(s)
		wrapWriteLn(s, "🐍 As you reach out, the rattlesnake strikes! You feel a sharp pain in your hand.")
		outPrintln(s)
		wrapWriteLn(s, "💀 The venom works quickly. GAME OVER.")
		s.IsPlaying = false
		return false
	}
	if s.OutlawRoom == s.CurrentRoom.ID && !safeVerbs[verb] && verb != "SHOOT" && verb != "KILL" {
		outPrintln(s)
		wrapWriteLn(s, "🤠 The outlaw doesn't like you poking around. He draws his gun and fires.")
		outPrintln(s)
		wrapWriteLn(s, "💥 Everything goes dark. GAME OVER.")
		s.IsPlaying = false
		return false
	}
	return true
}

var commands = []commandEntry{
	{"N", nil},
	{"NORTH", nil},
	{"S", nil},
	{"SOUTH", nil},
	{"E", nil},
	{"EAST", nil},
	{"W", nil},
	{"WEST", nil},
	{"LOOK", cmdHandleLook},
	{"L", cmdHandleLook},
	{"EXAMINE", cmdExamineItem},
	{"X", cmdExamineItem},
	{"SEARCH", cmdHandleSearch},
	{"HELP", cmdShowHelp},
	{"?", cmdShowHelp},
	{"H", cmdShowHelp},
	{"INVENTORY", cmdHandleInventory},
	{"I", cmdHandleInventory},
	{"INV", cmdHandleInventory},
	{"DRINK", cmdDrink},
	{"FILL", cmdFillCanteen},
	{"WATER", cmdWaterHorse},
	{"LIGHT", cmdLightLamp},
	{"FIX", cmdFixSomething},
	{"SADDLE", cmdSaddleHorse},
	{"PUT", cmdHandlePut},
	{"CLIMB", cmdHandleClimb},
	{"SAVE", cmdHandleSave},
	{"LOAD", cmdHandleLoad},
	{"DROP", cmdHandleDrop},
	{"D", cmdHandleDrop},
	{"BURN", cmdHandleBurn},
	{"FIRE", cmdHandleFire},
}

func processCommand(s *GameState, cmd string) {
	verb, noun := splitCommand(cmd)
	consumeTurn := true

	if !checkHazards(s, verb) {
		return
	}

	switch verb {
	case "MOUNT", "RIDE":
		cmdHandleMount(s, noun, &consumeTurn)
	case "DISMOUNT":
		cmdHandleDismount(s, noun, &consumeTurn)
	case "OPEN":
		cmdHandleOpen(s, strings.ToUpper(strings.TrimSpace(noun)), &consumeTurn)
	case "SHOOT", "KILL":
		cmdHandleShoot(s, noun, &consumeTurn)
	case "FREEZE", "WAIT":
		cmdHandleFreeze(s, noun, &consumeTurn)
	case "CHECK":
		nounUpper := strings.ToUpper(strings.TrimSpace(noun))
		if nounUpper == "INVENTORY" || nounUpper == "INV" || nounUpper == "I" {
			cmdHandleInventory(s, noun, &consumeTurn)
		}
	case "SCORE":
		cmdHandleScore(s, noun, &consumeTurn)
	case "TAKE", "GET":
		cmdHandleTake(s, noun, &consumeTurn)
	case "QUIT", "Q":
		cmdHandleQuit(s, noun, &consumeTurn)
	default:
		handled := false
		for _, entry := range commands {
			if verb == entry.verb {
				if entry.handler == nil {
					// Direction commands
					switch verb {
					case "N", "NORTH":
						if s.CurrentRoom.North != nil {
							printMovement(s, "NORTH", s.IsRiding)
						}
						moveTo(s, s.CurrentRoom.North)
					case "S", "SOUTH":
						if s.CurrentRoom.South != nil {
							printMovement(s, "SOUTH", s.IsRiding)
						}
						moveTo(s, s.CurrentRoom.South)
					case "E", "EAST":
						if s.CurrentRoom.East != nil {
							printMovement(s, "EAST", s.IsRiding)
						}
						moveTo(s, s.CurrentRoom.East)
					case "W", "WEST":
						if s.CurrentRoom.West != nil {
							printMovement(s, "WEST", s.IsRiding)
						}
						moveTo(s, s.CurrentRoom.West)
					}
				} else {
					entry.handler(s, noun, &consumeTurn)
				}
				handled = true
				break
			}
		}
		if !handled {
			outPrintln(s, "🤷 I don't know how to do that.")
		}
	}

	if s.IsPlaying && consumeTurn {
		updateWorld(s)
	}
}
