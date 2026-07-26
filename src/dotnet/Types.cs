namespace Dustwood;

public static class Constants
{
    public const int MaxRooms = 20;
    public const int MaxItems = 24;
    public const int InvLocation = -1;
    public const int MaxHistory = 10;
    public const int MaxCarry = 5;
    public const int ThirstLimit = 50;
    public const int HorseThirstLimit = 40;
    public const int DarkTurn = 30;
    public const int TwilightTurn = 20;

    public const int ScoreRoomVisit = 5;
    public const int ScoreItemPickup = 3;
    public const int ScoreNoteFound = 5;
    public const int ScorePumpFix = 20;
    public const int ScoreFirstFill = 10;
    public const int ScoreLampLight = 5;
    public const int ScoreBoxOpen = 10;
    public const int ScoreOutlawKill = 15;
    public const int ScoreTelegraphFix = 10;

    public const int StreamRoomID = 13;
    public const int DesertEntryRoomID = 8;
}

public class Room
{
    public int Id { get; set; }
    public string Name { get; set; } = "";
    public string Description { get; set; } = "";
    public Room? North { get; set; }
    public Room? South { get; set; }
    public Room? East { get; set; }
    public Room? West { get; set; }
}

public class Item
{
    public string Name { get; set; } = "";
    public string Description { get; set; } = "";
    public string Details { get; set; } = "";
    public int Location { get; set; }
    public bool IsTakeable { get; set; }
}
