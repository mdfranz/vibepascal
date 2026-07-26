namespace Dustwood;

using System.Text.Json;
using System.Text.Json.Serialization;

public class GameSummary
{
    [JsonPropertyName("room_id")]
    public int RoomID { get; set; }

    [JsonPropertyName("room_name")]
    public string RoomName { get; set; } = "";

    [JsonPropertyName("turns")]
    public int Turns { get; set; }

    [JsonPropertyName("score")]
    public int Score { get; set; }

    [JsonPropertyName("is_playing")]
    public bool IsPlaying { get; set; }

    [JsonPropertyName("is_riding")]
    public bool IsRiding { get; set; }

    [JsonPropertyName("is_dark")]
    public bool IsDark { get; set; }

    [JsonPropertyName("thirst")]
    public int Thirst { get; set; }

    [JsonPropertyName("horse_thirst")]
    public int HorseThirst { get; set; }

    [JsonPropertyName("has_water")]
    public bool HasWater { get; set; }

    [JsonPropertyName("lamp_lit")]
    public bool LampLit { get; set; }

    [JsonPropertyName("horse_saddled")]
    public bool HorseSaddled { get; set; }

    [JsonPropertyName("inventory")]
    public List<string> Inventory { get; set; } = new();
}

public static class SummaryHelper
{
    public static GameSummary SummarizeState(GameState s)
    {
        var summary = new GameSummary
        {
            RoomID = s.CurrentRoom?.Id ?? 0,
            RoomName = s.CurrentRoom?.Name ?? "",
            Turns = s.Turns,
            Score = s.Score,
            IsPlaying = s.IsPlaying,
            IsRiding = s.IsRiding,
            IsDark = IsDark(s),
            Thirst = s.Thirst,
            HorseThirst = s.HorseThirst,
            HasWater = s.HasWater,
            LampLit = s.IsLampLit,
            HorseSaddled = s.IsHorseSaddled
        };

        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Location == Constants.InvLocation)
            {
                summary.Inventory.Add(s.Items[i].Description);
            }
        }

        return summary;
    }

    public static bool IsDark(GameState s)
    {
        if (s.CurrentRoom == null) return false;
        // Room 4 (Assayer's Office) or Room 7 (Sheriff's Cell) or turn >= DarkTurn
        if (s.CurrentRoom.Id == 4 || s.CurrentRoom.Id == 7)
        {
            return !s.IsLampLit && s.TempLightTurns <= 0;
        }
        if (s.Turns >= Constants.DarkTurn)
        {
            return !s.IsLampLit && s.TempLightTurns <= 0;
        }
        return false;
    }

    public static string DescribeRoom(GameState s)
    {
        if (s.CurrentRoom == null) return "You are in an unknown location.";

        var sb = new System.Text.StringBuilder();
        sb.AppendLine(s.CurrentRoom.Name);
        sb.AppendLine(s.CurrentRoom.Description);

        var items = new List<string>();
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Location == s.CurrentRoom.Id)
            {
                items.Add(s.Items[i].Description);
            }
        }
        if (items.Count > 0)
        {
            sb.AppendLine("Items: " + string.Join(", ", items));
        }

        var exits = new List<string>();
        if (s.CurrentRoom.North != null) exits.Add("north");
        if (s.CurrentRoom.South != null) exits.Add("south");
        if (s.CurrentRoom.East != null) exits.Add("east");
        if (s.CurrentRoom.West != null) exits.Add("west");

        if (exits.Count > 0)
        {
            sb.Append("Exits: " + string.Join(", ", exits));
        }

        return sb.ToString().Trim();
    }

    public static string DescribeInventory(GameState s)
    {
        var items = new List<string>();
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Location == Constants.InvLocation)
            {
                items.Add(s.Items[i].Description);
            }
        }

        if (items.Count == 0) return "You are carrying nothing.";
        return "You are carrying: " + string.Join(", ", items);
    }

    public static string SummarizeStateJSON(GameState s)
    {
        var summary = SummarizeState(s);
        return JsonSerializer.Serialize(summary);
    }
}
