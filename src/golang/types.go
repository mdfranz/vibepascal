package main

const (
	MaxRooms          = 20
	MaxItems          = 24
	InvLocation       = -1
	MaxHistory        = 10
	MaxCarry          = 5
	ThirstLimit       = 50
	HorseThirstLimit  = 40
	DarkTurn          = 30
	TwilightTurn      = 20
	ScoreRoomVisit    = 5
	ScoreItemPickup   = 3
	ScoreNoteFound    = 5
	ScorePumpFix      = 20
	ScoreFirstFill    = 10
	ScoreLampLight    = 5
	ScoreBoxOpen      = 10
	ScoreOutlawKill   = 15
	ScoreTelegraphFix = 10
	StreamRoomID      = 13
	DesertEntryRoomID = 8
)

type Room struct {
	ID          int
	Name        string
	Description string
	North       *Room
	South       *Room
	East        *Room
	West        *Room
}

type Item struct {
	Name        string
	Description string
	Details     string
	Location    int
	IsTakeable  bool
}
