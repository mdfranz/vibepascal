namespace Dustwood;

public class GameState
{
    public Room?[] RoomRegistry { get; set; } = new Room?[Constants.MaxRooms + 1];
    public Item[] Items { get; set; } = new Item[Constants.MaxItems + 1];
    public Room? CurrentRoom { get; set; }

    public TextWriter Out { get; set; } = Console.Out;

    public bool IsPlaying { get; set; } = true;
    public bool IsPumpFixed { get; set; }
    public bool IsLampLit { get; set; }
    public bool HasWater { get; set; }
    public bool IsHeadless { get; set; }
    public bool IsBoxOpen { get; set; }
    public bool IsTelegraphFixed { get; set; }

    public bool IsHorseSaddled { get; set; }
    public bool IsRiding { get; set; }

    public int TempLightTurns { get; set; }
    public int CanteenDrinks { get; set; }
    public int SnakeRoom { get; set; }
    public int OutlawRoom { get; set; }
    public int Thirst { get; set; }
    public int Turns { get; set; }
    public int TurnLimit { get; set; } = 25;
    public int HorseThirst { get; set; }
    public int Score { get; set; }

    public bool[] RoomVisited { get; set; } = new bool[Constants.MaxRooms + 1];
    public bool[] ItemScored { get; set; } = new bool[Constants.MaxItems + 1];

    public bool ScoredPumpFix { get; set; }
    public bool ScoredFirstFill { get; set; }
    public bool ScoredLampLight { get; set; }
    public bool ScoredBoxOpen { get; set; }
    public bool ScoredTelegraphFix { get; set; }
    public bool ScoredOutlawKill { get; set; }
    public bool ScoredNoteFound { get; set; }

    public int[] RoomBurning { get; set; } = new int[Constants.MaxRooms + 1];
    public string[] History { get; set; } = new string[Constants.MaxHistory + 1];
    public int HistoryCount { get; set; }

    public bool AutosaveEnabled { get; set; }
    public int AutosaveInterval { get; set; } = 5;
    public string AutosavePath { get; set; } = "data/autosave.db";

    public GameState()
    {
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            Items[i] = new Item();
        }
        for (int i = 0; i <= Constants.MaxHistory; i++)
        {
            History[i] = "";
        }
    }
}
