package main

import (
	"fmt"
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

func printMovement(direction string, isRiding bool) {
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
	fmt.Println(emoji + msg)
}

func moveTo(s *GameState, newRoom *Room) {
	if newRoom == nil {
		fmt.Println("You cannot go that way.")
	} else if s.IsRiding && (newRoom.ID == 2 || newRoom.ID == 4 || newRoom.ID == 5 || newRoom.ID == 7) {
		fmt.Println("You can't bring a horse in there. Dismount first.")
	} else if s.CurrentRoom.ID == 6 && newRoom.ID == DesertEntryRoomID && !s.IsRiding {
		fmt.Println("The desert is too dangerous on foot. You must be riding a saddled horse.")
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
				fmt.Printf("🔥 The fire destroys %s.\n", s.Items[i].Description)
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
		fmt.Println("🔥 The fire drives away the rattlesnake.")
	}
	if s.SnakeRoom > 0 && rand.Intn(100) < 30 {
		s.SnakeRoom = 0
	}

	if s.Thirst > ThirstLimit-5 {
		fmt.Println()
		fmt.Println("🌵 === Your throat is parched. You need water soon. ===")
	}
	if s.IsHorseSaddled && isDesertRoom(s.CurrentRoom.ID) && s.HorseThirst > HorseThirstLimit-5 {
		fmt.Println()
		fmt.Println("🐎 === Your horse is showing signs of exhaustion. It needs water soon. ===")
	}

	if s.Thirst >= ThirstLimit {
		fmt.Println()
		wrapWriteLn("💀 You have collapsed from dehydration. GAME OVER.")
		s.IsPlaying = false
	}

	if isDesertRoom(s.CurrentRoom.ID) && !s.IsRiding {
		fmt.Println()
		wrapWriteLn("🔥 The desert heat is overwhelming on foot. You collapse into the sand. GAME OVER.")
		s.IsPlaying = false
	}

	if s.IsHorseSaddled && isDesertRoom(s.CurrentRoom.ID) && s.HorseThirst >= HorseThirstLimit {
		fmt.Println()
		wrapWriteLn("💀 Your horse collapses from dehydration. You are stranded in the desert. GAME OVER.")
		s.IsPlaying = false
	}

	if s.Turns == TwilightTurn {
		fmt.Println("🌇 The sun is getting low.")
	}
	if s.Turns == DarkTurn {
		fmt.Println("🌑 It is now dark.")
	}
}

func cmdDrink(s *GameState, noun string, consumeTurn *bool) {
	if findItem("CANTEEN", InvLocation, s) == 0 {
		fmt.Println("You don't have anything to drink from.")
	} else if !s.HasWater {
		fmt.Println("Your canteen is empty.")
	} else {
		s.Thirst = 0
		if s.CanteenDrinks > 0 {
			s.CanteenDrinks--
		}
		if s.CanteenDrinks <= 0 {
			s.HasWater = false
		}
		wrapWriteLn("💧 The water is warm but refreshing.")
		fmt.Println("Your thirst is quenched.")
	}
}

func cmdFillCanteen(s *GameState, noun string, consumeTurn *bool) {
	if findItem("CANTEEN", InvLocation, s) == 0 {
		fmt.Println("You have nothing to fill.")
	} else if s.CurrentRoom.ID == 3 && s.IsPumpFixed {
		s.HasWater = true
		s.CanteenDrinks = 3
		fmt.Println("💧 You fill your canteen with fresh water from the pump.")
		if !s.ScoredFirstFill {
			s.ScoredFirstFill = true
			s.Score += ScoreFirstFill
		}
	} else if s.CurrentRoom.ID == StreamRoomID {
		s.HasWater = true
		s.CanteenDrinks = 3
		fmt.Println("💧 You fill your canteen with cold stream water.")
	} else {
		fmt.Println("There is no water here.")
	}
}

func cmdLightLamp(s *GameState, noun string, consumeTurn *bool) {
	nounUpper := strings.ToUpper(strings.TrimSpace(noun))
	if noun != "" && nounUpper != "MATCH" && nounUpper != "MATCHES" {
		fmt.Println("Light what?")
		return
	}
	if findItem("LAMP", InvLocation, s) > 0 {
		s.IsLampLit = true
		wrapWriteLn("🔦 You light the lamp. A yellow glow illuminates the room.")
		if !s.ScoredLampLight {
			s.ScoredLampLight = true
			s.Score += ScoreLampLight
		}
	} else {
		s.TempLightTurns = 3
		wrapWriteLn("🔥 You strike a match. The room brightens for a moment.")
	}
}

func cmdShowHelp(s *GameState, noun string, consumeTurn *bool) {
	fmt.Println()
	fmt.Println("Available Commands:")
	fmt.Println("  🚶 N, S, E, W      - Move North, South, East, West")
	fmt.Println("  👀 LOOK (L)        - Look around")
	fmt.Println("  🔍 EXAMINE (X)     - Look closely at an item")
	fmt.Println("  🖐️  TAKE (GET)      - Pick up an item")
	fmt.Println("  ✋  DROP            - Leave an item")
	fmt.Println("  🎒 INVENTORY (I)   - Check your gear")
	fmt.Println("  💧 DRINK           - Drink from your canteen")
	fmt.Println("  🚰 FILL            - Fill canteen at a water source")
	fmt.Println("  🐎 WATER           - Water your horse at a water source")
	fmt.Println("  🔦 LIGHT           - Light your lamp if you have matches")
	fmt.Println("  🔧 FIX             - Repair something")
	fmt.Println("  🏇 SADDLE          - Put a saddle on the horse")
	fmt.Println("  ❄️  FREEZE (WAIT)   - Stay still to avoid danger")
	fmt.Println("  🔥 BURN            - Burn a flammable item (requires matches)")
	fmt.Println("  🔥 FIRE            - Start a fire in certain rooms (requires matches)")
	fmt.Println("  🧗 CLIMB           - Climb a steep obstacle")
	fmt.Println("  💾 SAVE / LOAD     - Save or load your progress")
	fmt.Println("  🏆 SCORE           - Show current score")
	fmt.Println("  ❓ HELP (H)        - Show this list")
	fmt.Println("  🚪 QUIT (Q)        - Exit")
	fmt.Println()
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
		wrapWriteLn(s.Items[itemID].Details)
		if s.Items[itemID].Name == "ROCK" {
			keyId := findItemAny("KEY", s)
			if keyId > 0 && s.Items[keyId].Location == 0 {
				s.Items[keyId].Location = s.CurrentRoom.ID
				fmt.Println()
				fmt.Println("You lift the rock. A small brass key is hidden beneath it.")
			}
		}
		if s.Items[itemID].Name == "BOOK" {
			noteId := findItemAny("NOTE", s)
			if noteId > 0 && s.Items[noteId].Location == 0 {
				s.Items[noteId].Location = InvLocation
				fmt.Println()
				fmt.Println("A small folded note falls out of the book.")
				if !s.ScoredNoteFound {
					s.ScoredNoteFound = true
					s.Score += ScoreNoteFound
				}
			}
		}
	} else if noun == "" {
		look(s)
	} else {
		fmt.Println("You don't see that here.")
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
			fmt.Println("You fix the pump. Water starts to flow.")
			s.Items[3].Description = "a working water pump"
			if !s.ScoredPumpFix {
				s.ScoredPumpFix = true
				s.Score += ScorePumpFix
			}
		} else {
			fmt.Println("You need leather.")
		}
	} else if (noun == "WIRE" || noun == "WIRES" || noun == "TELEGRAPH") && s.CurrentRoom.ID == 2 {
		if s.IsTelegraphFixed {
			fmt.Println("The telegraph is already repaired.")
		} else if findItem("WIRE", InvLocation, s) > 0 {
			s.IsTelegraphFixed = true
			fmt.Println("You splice the copper wire and restore the telegraph line.")
			if s.RoomRegistry[2] != nil {
				s.RoomRegistry[2].Description = "The telegraph has been repaired. The line hums faintly with life."
			}
			if !s.ScoredTelegraphFix {
				s.ScoredTelegraphFix = true
				s.Score += ScoreTelegraphFix
			}
			s.Items[4].Location = 0
		} else {
			fmt.Println("You need copper wire.")
		}
	} else {
		fmt.Println("Nothing to fix here.")
	}
}

func cmdWaterHorse(s *GameState, targetNoun string, consumeTurn *bool) {
	noun := strings.ToUpper(strings.TrimSpace(targetNoun))
	if noun != "" && noun != "HORSE" && noun != "MARE" {
		fmt.Println("Water what?")
		return
	}
	if !s.IsHorseSaddled {
		fmt.Println("You don't have a horse with you.")
		return
	}
	if s.CurrentRoom.ID != StreamRoomID {
		fmt.Println("There is no water here for your horse.")
		return
	}
	s.HorseThirst = 0
	fmt.Println("Your horse drinks deeply from the stream.")
}

func cmdSaddleHorse(s *GameState, targetNoun string, consumeTurn *bool) {
	noun := strings.ToUpper(strings.TrimSpace(targetNoun))
	if noun != "" && noun != "HORSE" && noun != "ON HORSE" && noun != "MARE" {
		fmt.Println("Saddle what?")
		return
	}
	horseID := findItem("HORSE", s.CurrentRoom.ID, s)
	if horseID == 0 {
		fmt.Println("There is no horse here.")
		return
	}
	saddleID := findItem("SADDLE", InvLocation, s)
	if saddleID == 0 {
		fmt.Println("You need a saddle.")
		return
	}
	if s.IsHorseSaddled {
		fmt.Println("The horse is already saddled.")
		return
	}
	s.IsHorseSaddled = true
	s.Items[saddleID].Location = 0
	s.Items[horseID].Description = "a saddled horse"
	s.Items[horseID].Details = "A calm, saddle-ready horse. It looks steady and patient."
	fmt.Println("You secure the saddle onto the horse. It stands quietly.")
}

func cmdHandleMount(s *GameState, noun string, consumeTurn *bool) {
	nounUpper := strings.ToUpper(strings.TrimSpace(noun))
	if nounUpper != "" && nounUpper != "HORSE" && nounUpper != "MARE" {
		fmt.Println("Mount what?")
		return
	}
	if s.IsRiding {
		fmt.Println("You are already riding.")
	} else if findItem("HORSE", s.CurrentRoom.ID, s) > 0 {
		if s.IsHorseSaddled {
			s.IsRiding = true
			horseID := findItem("HORSE", s.CurrentRoom.ID, s)
			s.Items[horseID].Location = InvLocation
			fmt.Println("You swing yourself into the saddle. You are now riding.")
		} else {
			fmt.Println("The horse needs a saddle before you can ride her.")
		}
	} else {
		fmt.Println("There is no horse here.")
	}
}

func cmdHandleDismount(s *GameState, noun string, consumeTurn *bool) {
	nounUpper := strings.ToUpper(strings.TrimSpace(noun))
	if nounUpper != "" && nounUpper != "HORSE" && nounUpper != "MARE" {
		fmt.Println("Dismount what?")
		return
	}
	if !s.IsRiding {
		fmt.Println("You aren't riding anything.")
	} else {
		s.IsRiding = false
		horseID := findItem("HORSE", InvLocation, s)
		if horseID > 0 {
			s.Items[horseID].Location = s.CurrentRoom.ID
		}
		fmt.Println("You dismount and stand beside your horse.")
	}
}

func cmdHandleOpen(s *GameState, noun string, consumeTurn *bool) {
	if noun == "BOX" && s.CurrentRoom.ID == 7 {
		if s.IsBoxOpen {
			fmt.Println("It is already open.")
		} else if findItem("KEY", InvLocation, s) == 0 {
			fmt.Println("The box is locked. You need a key.")
		} else {
			s.IsBoxOpen = true
			s.Items[8].Location = 7
			fmt.Println("You unlock the box. Inside lies a heavy revolver.")
			if !s.ScoredBoxOpen {
				s.ScoredBoxOpen = true
				s.Score += ScoreBoxOpen
			}
		}
	} else {
		fmt.Println("There is nothing to open here.")
		*consumeTurn = false
	}
}

func cmdHandleShoot(s *GameState, noun string, consumeTurn *bool) {
	if findItem("REVOLVER", InvLocation, s) == 0 {
		fmt.Println("You have nothing to shoot with.")
	} else if s.OutlawRoom == s.CurrentRoom.ID {
		s.OutlawRoom = 0
		wrapWriteLn("💥 You draw your revolver and fire first. The outlaw falls to the ground.")
		fmt.Println("💀 The threat is gone.")
		if !s.ScoredOutlawKill {
			s.ScoredOutlawKill = true
			s.Score += ScoreOutlawKill
		}
	} else {
		fmt.Println("Nothing here to shoot.")
	}
}

func cmdHandleFreeze(s *GameState, noun string, consumeTurn *bool) {
	fmt.Println("You stay perfectly still. The snake watches you...")
	if rand.Intn(100) < 50 {
		s.SnakeRoom = 0
		fmt.Println("The snake loses interest and slithers into the shadows.")
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
	fmt.Println("You are carrying:")
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == InvLocation {
			fmt.Printf("  - %s\n", s.Items[i].Description)
		}
	}
	*consumeTurn = false
}

func cmdHandleScore(s *GameState, noun string, consumeTurn *bool) {
	fmt.Printf("🏆 Score: %d\n", s.Score)
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
			fmt.Println("You can't carry any more. Drop something first.")
			return
		}
		if !s.Items[itemID].IsTakeable {
			switch s.Items[itemID].Name {
			case "PUMP":
				fmt.Println("The pump is fixed in place.")
			case "HORSE":
				fmt.Println("It's too big to carry.")
			case "BOX":
				fmt.Println("It's bolted down.")
			case "ROCK":
				fmt.Println("It's too heavy to carry.")
			default:
				fmt.Println("You can't take that.")
			}
			return
		}
		s.Items[itemID].Location = InvLocation
		fmt.Printf("🎒 Taken: %s.\n", s.Items[itemID].Description)
		if !s.ItemScored[itemID] {
			s.ItemScored[itemID] = true
			s.Score += ScoreItemPickup
		}
	} else {
		fmt.Println("Not here.")
	}
}

func cmdHandleQuit(s *GameState, noun string, consumeTurn *bool) {
	s.IsPlaying = false
}

func cmdHandleClimb(s *GameState, noun string, consumeTurn *bool) {
	if s.CurrentRoom.ID == 12 {
		moveTo(s, s.RoomRegistry[StreamRoomID])
	} else {
		fmt.Println("There is nothing to climb here.")
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
		fmt.Printf("✋ Dropped: %s.\n", s.Items[itemID].Description)
	} else {
		fmt.Println("You aren't carrying that.")
	}
}

func cmdHandleBurn(s *GameState, noun string, consumeTurn *bool) {
	target := strings.ToUpper(strings.TrimSpace(noun))
	if target == "" {
		fmt.Println("Burn what?")
		return
	}
	if findItem("MATCHES", InvLocation, s) == 0 {
		fmt.Println("You have nothing to burn it with.")
		return
	}
	itemID := findItem(target, InvLocation, s)
	if itemID == 0 {
		itemID = findItem(target, s.CurrentRoom.ID, s)
	}
	if itemID == 0 {
		fmt.Println("You don't see that here.")
		return
	}
	name := s.Items[itemID].Name
	if name != "BOOK" && name != "LEDGER" && name != "LEATHER" && name != "MAP" && name != "SADDLE" {
		fmt.Println("It doesn't burn.")
		return
	}
	s.Items[itemID].Location = 0
	fmt.Println("You burn it to ash.")
}

func cmdHandleFire(s *GameState, noun string, consumeTurn *bool) {
	if findItem("MATCHES", InvLocation, s) == 0 {
		fmt.Println("You have nothing to start a fire with.")
		return
	}
	id := s.CurrentRoom.ID
	if id != 2 && id != 3 && id != 5 {
		fmt.Println("There is nothing here that will catch fire.")
		return
	}
	if s.RoomBurning[id] > 0 {
		fmt.Println("A fire is already burning here.")
		return
	}
	s.RoomBurning[id] = 3
	fmt.Println("🔥 You start a fire. The room glows with heat.")
	if s.SnakeRoom == id {
		s.SnakeRoom = 0
		fmt.Println("🔥 The rattlesnake recoils from the flames and disappears.")
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
		fmt.Println()
		wrapWriteLn("🐍 As you reach out, the rattlesnake strikes! You feel a sharp pain in your hand.")
		fmt.Println()
		wrapWriteLn("💀 The venom works quickly. GAME OVER.")
		s.IsPlaying = false
		return false
	}
	if s.OutlawRoom == s.CurrentRoom.ID && !safeVerbs[verb] && verb != "SHOOT" && verb != "KILL" {
		fmt.Println()
		wrapWriteLn("🤠 The outlaw doesn't like you poking around. He draws his gun and fires.")
		fmt.Println()
		wrapWriteLn("💥 Everything goes dark. GAME OVER.")
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
							printMovement("NORTH", s.IsRiding)
						}
						moveTo(s, s.CurrentRoom.North)
					case "S", "SOUTH":
						if s.CurrentRoom.South != nil {
							printMovement("SOUTH", s.IsRiding)
						}
						moveTo(s, s.CurrentRoom.South)
					case "E", "EAST":
						if s.CurrentRoom.East != nil {
							printMovement("EAST", s.IsRiding)
						}
						moveTo(s, s.CurrentRoom.East)
					case "W", "WEST":
						if s.CurrentRoom.West != nil {
							printMovement("WEST", s.IsRiding)
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
			fmt.Println("🤷 I don't know how to do that.")
		}
	}

	if s.IsPlaying && consumeTurn {
		updateWorld(s)
	}
}
