namespace Dustwood;

public static class IO
{
    public static void OutPrint(GameState s, string text)
    {
        s.Out.Write(text);
    }

    public static void OutPrintln(GameState s, string text = "")
    {
        s.Out.WriteLine(text);
    }

    public static void OutPrintf(GameState s, string format, params object[] args)
    {
        s.Out.Write(string.Format(format, args));
    }

    public static void WrapWriteLn(GameState s, string text)
    {
        const int maxWidth = 79;
        while (text.Length > maxWidth)
        {
            int spacePos = maxWidth;
            while (spacePos > 0 && text[spacePos] != ' ')
            {
                spacePos--;
            }
            if (spacePos == 0)
            {
                spacePos = maxWidth;
            }
            OutPrintln(s, text.Substring(0, spacePos));
            text = text.Substring(spacePos).TrimStart(' ');
        }
        OutPrintln(s, text);
    }

    public static bool IsDark(GameState s)
    {
        return SummaryHelper.IsDark(s);
    }

    public static void Look(GameState s)
    {
        OutPrintln(s);
        if (IsDark(s))
        {
            WrapWriteLn(s, "🌑 It is pitch black. You can't see anything.");
            return;
        }

        if (s.CurrentRoom == null) return;

        if (s.RoomBurning[s.CurrentRoom.Id] > 0)
        {
            OutPrintln(s, "🔥 The room is lit by a growing fire.");
        }

        if (s.Turns >= Constants.DarkTurn)
        {
            OutPrintln(s, "🌕 [The moon hangs in the black sky]");
        }
        else if (s.Turns >= Constants.TwilightTurn)
        {
            OutPrintln(s, "🌇 [The sky is purple as the sun sets]");
        }

        if (s.IsRiding)
        {
            OutPrint(s, "🏇 ");
        }
        OutPrintf(s, "📍 === {0} === ", s.CurrentRoom.Name);
        switch (s.CurrentRoom.Id)
        {
            case 1: OutPrint(s, "🏘️"); break;
            case 2: OutPrint(s, "📟"); break;
            case 3: OutPrint(s, "🐴"); break;
            case 4: OutPrint(s, "⚖️"); break;
            case 5: OutPrint(s, "🛒"); break;
            case 6: OutPrint(s, "🌵"); break;
            case 7: OutPrint(s, "👮"); break;
            case 8: case 9: case 10: case 11: case 12: case 13:
                OutPrint(s, "🏜️"); break;
        }
        OutPrintln(s);
        WrapWriteLn(s, s.CurrentRoom.Description);

        if (Commands.FindItemInLoc("MAP", Constants.InvLocation, s) > 0)
        {
            var exits = new List<string>();
            if (s.CurrentRoom.North != null) exits.Add("NORTH");
            if (s.CurrentRoom.South != null) exits.Add("SOUTH");
            if (s.CurrentRoom.East != null) exits.Add("EAST");
            if (s.CurrentRoom.West != null) exits.Add("WEST");
            if (exits.Count > 0)
            {
                OutPrintf(s, "Exits: [{0}]\n", string.Join(", ", exits));
            }
        }

        if (s.SnakeRoom == s.CurrentRoom.Id)
        {
            OutPrintln(s);
            OutPrintln(s, "!!! A RATTLESNAKE is coiled here, buzzing its tail angrily !!!");
            OutPrintln(s, "One wrong move could be your last.");
        }

        if (s.OutlawRoom == s.CurrentRoom.Id)
        {
            OutPrintln(s);
            OutPrintln(s, "!!! A DIRTY OUTLAW is leaning against the wall, hand on his holster !!!");
            OutPrintln(s, "\"You don't belong here, stranger,\" he sneers.");
        }

        bool foundItems = false;
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Location == s.CurrentRoom.Id)
            {
                if (!foundItems)
                {
                    OutPrintln(s);
                    OutPrintln(s, "📦 You see the following here:");
                    foundItems = true;
                }
                OutPrintf(s, "  - {0}\n", s.Items[i].Description);
            }
        }
        OutPrintln(s);
    }

    public static string CustomReadLn(GameState s, string prompt)
    {
        OutPrint(s, prompt);
        string? line = Console.ReadLine();
        if (line == null) return "QUIT";
        return line.TrimEnd('\r', '\n');
    }
}
