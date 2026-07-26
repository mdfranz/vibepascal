namespace Dustwood;

public static class World
{
    public static void LoadWorld(GameState s, string path)
    {
        if (!File.Exists(path))
        {
            s.Out.WriteLine($"Error loading world: file not found at {path}");
            return;
        }

        var iniData = ParseIni(path);

        // First pass: create rooms
        for (int i = 1; i <= Constants.MaxRooms; i++)
        {
            string sectionName = $"Room{i}";
            if (!iniData.TryGetValue(sectionName, out var section))
                continue;

            var r = new Room
            {
                Id = i,
                Name = section.GetValueOrDefault("Name", ""),
                Description = section.GetValueOrDefault("Description", "")
            };
            s.RoomRegistry[i] = r;
        }

        // Second pass: link exits
        for (int i = 1; i <= Constants.MaxRooms; i++ )
        {
            if (s.RoomRegistry[i] == null)
                continue;

            string sectionName = $"Room{i}";
            if (!iniData.TryGetValue(sectionName, out var section))
                continue;

            int n = GetInt(section, "North");
            int so = GetInt(section, "South");
            int e = GetInt(section, "East");
            int w = GetInt(section, "West");

            if (n > 0 && n <= Constants.MaxRooms) s.RoomRegistry[i]!.North = s.RoomRegistry[n];
            if (so > 0 && so <= Constants.MaxRooms) s.RoomRegistry[i]!.South = s.RoomRegistry[so];
            if (e > 0 && e <= Constants.MaxRooms) s.RoomRegistry[i]!.East = s.RoomRegistry[e];
            if (w > 0 && w <= Constants.MaxRooms) s.RoomRegistry[i]!.West = s.RoomRegistry[w];
        }

        // Load items
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            string sectionName = $"Item{i}";
            if (!iniData.TryGetValue(sectionName, out var section))
                continue;

            s.Items[i].Name = section.GetValueOrDefault("Name", "").ToUpperInvariant();
            s.Items[i].Description = section.GetValueOrDefault("Description", "");
            s.Items[i].Details = section.GetValueOrDefault("Details", "");
            s.Items[i].Location = GetInt(section, "Location");
            s.Items[i].IsTakeable = GetInt(section, "IsTakeable") == 1;
        }
    }

    public static void RandomizeMapLocation(GameState s, Random rng)
    {
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Name == "MAP")
            {
                s.Items[i].Location = rng.Next(1, 8); // 1 to 7
                break;
            }
        }
    }

    private static int GetInt(Dictionary<string, string> section, string key)
    {
        if (section.TryGetValue(key, out var val) && int.TryParse(val, out var result))
        {
            return result;
        }
        return 0;
    }

    private static Dictionary<string, Dictionary<string, string>> ParseIni(string path)
    {
        var result = new Dictionary<string, Dictionary<string, string>>(StringComparer.OrdinalIgnoreCase);
        Dictionary<string, string>? currentSection = null;

        foreach (var rawLine in File.ReadAllLines(path))
        {
            var line = rawLine.Trim();
            if (string.IsNullOrWhiteSpace(line) || line.StartsWith(';') || line.StartsWith('#'))
                continue;

            if (line.StartsWith('[') && line.EndsWith(']'))
            {
                var sectionName = line.Substring(1, line.Length - 2).Trim();
                currentSection = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                result[sectionName] = currentSection;
            }
            else if (currentSection != null)
            {
                int eqIdx = line.IndexOf('=');
                if (eqIdx > 0)
                {
                    var key = line.Substring(0, eqIdx).Trim();
                    var val = line.Substring(eqIdx + 1).Trim();
                    currentSection[key] = val;
                }
            }
        }

        return result;
    }
}
