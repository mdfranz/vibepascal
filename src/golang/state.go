package main

type GameState struct {
	RoomRegistry [MaxRooms + 1]*Room
	Items        [MaxItems + 1]Item
	CurrentRoom  *Room

	IsPlaying       bool
	IsPumpFixed     bool
	IsLampLit       bool
	HasWater        bool
	IsHeadless      bool
	IsBoxOpen       bool
	IsTelegraphFixed bool

	IsHorseSaddled bool
	IsRiding       bool

	TempLightTurns int
	CanteenDrinks  int
	SnakeRoom      int
	OutlawRoom     int
	Thirst         int
	Turns          int
	HorseThirst    int
	Score          int

	RoomVisited [MaxRooms + 1]bool
	ItemScored  [MaxItems + 1]bool

	ScoredPumpFix      bool
	ScoredFirstFill    bool
	ScoredLampLight    bool
	ScoredBoxOpen      bool
	ScoredTelegraphFix bool
	ScoredOutlawKill   bool
	ScoredNoteFound    bool

	RoomBurning [MaxRooms + 1]int
	History     [MaxHistory + 1]string
	HistoryCount int
}

func initState(s *GameState) {
	for i := 1; i <= MaxRooms; i++ {
		s.RoomRegistry[i] = nil
	}
	for i := 1; i <= MaxItems; i++ {
		s.Items[i].Name = ""
		s.Items[i].Description = ""
		s.Items[i].Details = ""
		s.Items[i].Location = 0
		s.Items[i].IsTakeable = false
	}
	s.CurrentRoom = nil
	s.IsPlaying = true
	s.IsPumpFixed = false
	s.IsLampLit = false
	s.HasWater = false
	s.IsHeadless = false
	s.IsBoxOpen = false
	s.IsTelegraphFixed = false
	s.TempLightTurns = 0
	s.CanteenDrinks = 0
	s.IsHorseSaddled = false
	s.IsRiding = false
	s.SnakeRoom = 0
	s.OutlawRoom = 0
	s.Thirst = 0
	s.Turns = 0
	s.HorseThirst = 0
	s.Score = 0
	for i := 1; i <= MaxRooms; i++ {
		s.RoomVisited[i] = false
	}
	for i := 1; i <= MaxItems; i++ {
		s.ItemScored[i] = false
	}
	s.ScoredPumpFix = false
	s.ScoredFirstFill = false
	s.ScoredLampLight = false
	s.ScoredBoxOpen = false
	s.ScoredTelegraphFix = false
	s.ScoredOutlawKill = false
	s.ScoredNoteFound = false
	for i := 1; i <= MaxRooms; i++ {
		s.RoomBurning[i] = 0
	}
	for i := 0; i <= MaxHistory; i++ {
		s.History[i] = ""
	}
	s.HistoryCount = 0
}
