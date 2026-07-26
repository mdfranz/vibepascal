namespace Dustwood;

using System.Text.Json;

public static class Persistence
{
    public class SaveStateData
    {
        public int CurrentRoomId { get; set; }
        public bool IsPumpFixed { get; set; }
        public bool IsLampLit { get; set; }
        public bool HasWater { get; set; }
        public bool IsHorseSaddled { get; set; }
        public bool IsRiding { get; set; }
        public bool IsTelegraphFixed { get; set; }
        public int TempLightTurns { get; set; }
        public int CanteenDrinks { get; set; }
        public int Thirst { get; set; }
        public int HorseThirst { get; set; }
        public int Turns { get; set; }
        public int Score { get; set; }
        public int[] RoomBurning { get; set; } = new int[Constants.MaxRooms + 1];

        public int[] ItemLocations { get; set; } = new int[Constants.MaxItems + 1];
        public string[] ItemDescriptions { get; set; } = new string[Constants.MaxItems + 1];

        public bool[] RoomVisited { get; set; } = new bool[Constants.MaxRooms + 1];
        public bool[] ItemScored { get; set; } = new bool[Constants.MaxItems + 1];

        public bool ScoredPumpFix { get; set; }
        public bool ScoredFirstFill { get; set; }
        public bool ScoredLampLight { get; set; }
        public bool ScoredBoxOpen { get; set; }
        public bool ScoredTelegraphFix { get; set; }
        public bool ScoredOutlawKill { get; set; }
        public bool ScoredNoteFound { get; set; }
    }

    public static void SaveGame(GameState s, string path)
    {
        SaveGameInternal(s, path, quiet: false);
    }

    public static void CheckAutosave(GameState s)
    {
        if (!s.AutosaveEnabled || s.AutosaveInterval <= 0) return;
        if (s.Turns > 0 && s.Turns % s.AutosaveInterval == 0)
        {
            SaveGameInternal(s, s.AutosavePath, quiet: true);
        }
    }

    public static void SaveGameInternal(GameState s, string path, bool quiet)
    {
        try
        {
            var dir = Path.GetDirectoryName(path);
            if (!string.IsNullOrEmpty(dir) && !Directory.Exists(dir))
            {
                Directory.CreateDirectory(dir);
            }

            var data = new SaveStateData
            {
                CurrentRoomId = s.CurrentRoom?.Id ?? 1,
                IsPumpFixed = s.IsPumpFixed,
                IsLampLit = s.IsLampLit,
                HasWater = s.HasWater,
                IsHorseSaddled = s.IsHorseSaddled,
                IsRiding = s.IsRiding,
                IsTelegraphFixed = s.IsTelegraphFixed,
                TempLightTurns = s.TempLightTurns,
                CanteenDrinks = s.CanteenDrinks,
                Thirst = s.Thirst,
                HorseThirst = s.HorseThirst,
                Turns = s.Turns,
                Score = s.Score,
                RoomBurning = (int[])s.RoomBurning.Clone(),
                RoomVisited = (bool[])s.RoomVisited.Clone(),
                ItemScored = (bool[])s.ItemScored.Clone(),
                ScoredPumpFix = s.ScoredPumpFix,
                ScoredFirstFill = s.ScoredFirstFill,
                ScoredLampLight = s.ScoredLampLight,
                ScoredBoxOpen = s.ScoredBoxOpen,
                ScoredTelegraphFix = s.ScoredTelegraphFix,
                ScoredOutlawKill = s.ScoredOutlawKill,
                ScoredNoteFound = s.ScoredNoteFound
            };

            for (int i = 1; i <= Constants.MaxItems; i++)
            {
                data.ItemLocations[i] = s.Items[i].Location;
                data.ItemDescriptions[i] = s.Items[i].Description;
            }

            string json = JsonSerializer.Serialize(data, new JsonSerializerOptions { WriteIndented = true });
            File.WriteAllText(path, json);

            if (!quiet)
            {
                s.Out.WriteLine("💾 Game saved.");
            }
        }
        catch (Exception ex)
        {
            if (!quiet)
            {
                s.Out.WriteLine($"Error saving game: {ex.Message}");
            }
        }
    }

    public static void LoadGame(GameState s, string path)
    {
        if (!File.Exists(path))
        {
            s.Out.WriteLine("No save file found.");
            return;
        }

        try
        {
            string json = File.ReadAllText(path);
            var data = JsonSerializer.Deserialize<SaveStateData>(json);
            if (data == null)
            {
                s.Out.WriteLine("Error loading save file: Invalid data format.");
                return;
            }

            if (data.CurrentRoomId >= 1 && data.CurrentRoomId <= Constants.MaxRooms && s.RoomRegistry[data.CurrentRoomId] != null)
            {
                s.CurrentRoom = s.RoomRegistry[data.CurrentRoomId];
            }

            s.IsPumpFixed = data.IsPumpFixed;
            s.IsLampLit = data.IsLampLit;
            s.HasWater = data.HasWater;
            s.IsHorseSaddled = data.IsHorseSaddled;
            s.IsRiding = data.IsRiding;
            s.IsTelegraphFixed = data.IsTelegraphFixed;
            s.TempLightTurns = data.TempLightTurns;
            s.CanteenDrinks = data.CanteenDrinks;
            s.Thirst = data.Thirst;
            s.HorseThirst = data.HorseThirst;
            s.Turns = data.Turns;
            s.Score = data.Score;
            s.RoomBurning = data.RoomBurning ?? s.RoomBurning;
            s.RoomVisited = data.RoomVisited ?? s.RoomVisited;
            s.ItemScored = data.ItemScored ?? s.ItemScored;

            s.ScoredPumpFix = data.ScoredPumpFix;
            s.ScoredFirstFill = data.ScoredFirstFill;
            s.ScoredLampLight = data.ScoredLampLight;
            s.ScoredBoxOpen = data.ScoredBoxOpen;
            s.ScoredTelegraphFix = data.ScoredTelegraphFix;
            s.ScoredOutlawKill = data.ScoredOutlawKill;
            s.ScoredNoteFound = data.ScoredNoteFound;

            if (data.ItemLocations != null && data.ItemDescriptions != null)
            {
                for (int i = 1; i <= Constants.MaxItems; i++)
                {
                    if (i < data.ItemLocations.Length) s.Items[i].Location = data.ItemLocations[i];
                    if (i < data.ItemDescriptions.Length && !string.IsNullOrEmpty(data.ItemDescriptions[i]))
                    {
                        s.Items[i].Description = data.ItemDescriptions[i];
                    }
                }
            }

            s.Out.WriteLine("💾 Game loaded.");
        }
        catch (Exception ex)
        {
            s.Out.WriteLine($"Error loading save file: {ex.Message}");
        }
    }
}
