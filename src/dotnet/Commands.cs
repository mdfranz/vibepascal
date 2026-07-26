namespace Dustwood;

public static class Commands
{
    private static readonly HashSet<string> SafeVerbs = new(StringComparer.OrdinalIgnoreCase)
    {
        "N", "S", "E", "W",
        "NORTH", "SOUTH", "EAST", "WEST",
        "LOOK", "L", "EXAMINE", "X",
        "SEARCH", "INVENTORY", "I",
        "CHECK", "HELP", "?", "H",
        "SCORE", "SAVE", "LOAD",
        "QUIT", "Q"
    };

    public static (string Verb, string Noun) SplitCommand(string cmd)
    {
        string trimmed = cmd.Trim();
        int spacePos = trimmed.IndexOf(' ');
        if (spacePos >= 0)
        {
            return (trimmed.Substring(0, spacePos).ToUpperInvariant(), trimmed.Substring(spacePos + 1).Trim());
        }
        return (trimmed.ToUpperInvariant(), "");
    }

    public static int FindItem(string name, int loc, GameState s)
    {
        string upper = name.ToUpperInvariant();
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Location == loc && s.Items[i].Name == upper)
            {
                return i;
            }
        }
        return 0;
    }

    public static int FindItemInLoc(string name, int loc, GameState s)
    {
        return FindItem(name, loc, s);
    }

    public static int FindItemAny(string name, GameState s)
    {
        string upper = name.ToUpperInvariant();
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Name == upper)
            {
                return i;
            }
        }
        return 0;
    }

    public static bool IsDesertRoom(int id)
    {
        return id >= 8 && id <= 13;
    }

    public static void PrintMovement(GameState s, string direction, bool isRiding, Random rng)
    {
        int idx = rng.Next(5);
        string emoji, msg;
        if (isRiding)
        {
            emoji = "🏇 ";
            msg = idx switch
            {
                0 => $"You ride {direction}.",
                1 => $"You guide your horse {direction}.",
                2 => $"You trot {direction}.",
                3 => $"You and your mare head {direction}.",
                _ => $"The horse carries you {direction}."
            };
        }
        else
        {
            emoji = "🚶 ";
            msg = idx switch
            {
                0 => $"You walk {direction}.",
                1 => $"You trek {direction} through the dust.",
                2 => $"You head {direction}.",
                3 => $"You make your way {direction}.",
                _ => $"You trudge {direction} across the dry ground."
            };
        }
        IO.OutPrintln(s, emoji + msg);
    }

    public static void MoveTo(GameState s, Room? newRoom, Random rng)
    {
        if (newRoom == null)
        {
            IO.OutPrintln(s, "You cannot go that way.");
        }
        else if (s.IsRiding && (newRoom.Id == 2 || newRoom.Id == 4 || newRoom.Id == 5 || newRoom.Id == 7))
        {
            IO.OutPrintln(s, "You can't bring a horse in there. Dismount first.");
        }
        else if (s.CurrentRoom?.Id == 6 && newRoom.Id == Constants.DesertEntryRoomID && !s.IsRiding)
        {
            IO.OutPrintln(s, "The desert is too dangerous on foot. You must be riding a saddled horse.");
        }
        else
        {
            s.CurrentRoom = newRoom;
            if (s.IsRiding)
            {
                for (int i = 1; i <= Constants.MaxItems; i++)
                {
                    if (s.Items[i].Name == "HORSE")
                    {
                        s.Items[i].Location = Constants.InvLocation;
                        break;
                    }
                }
            }

            if (s.CurrentRoom.Id != 1 && !s.RoomVisited[s.CurrentRoom.Id])
            {
                s.RoomVisited[s.CurrentRoom.Id] = true;
                s.Score += Constants.ScoreRoomVisit;
            }

            if (s.CurrentRoom.Id != 1 && rng.Next(100) < 20)
            {
                s.SnakeRoom = s.CurrentRoom.Id;
            }
            else
            {
                s.SnakeRoom = 0;
            }

            if (s.CurrentRoom.Id != 1 && s.CurrentRoom.Id != 7 && rng.Next(100) < 15)
            {
                s.OutlawRoom = s.CurrentRoom.Id;
            }
            else
            {
                s.OutlawRoom = 0;
            }

            IO.Look(s);
        }
    }

    public static void UpdateWorld(GameState s, Random rng)
    {
        s.Turns++;
        s.Thirst++;
        if (s.TempLightTurns > 0 && !s.IsLampLit)
        {
            s.TempLightTurns--;
        }
        if (s.IsHorseSaddled && s.CurrentRoom != null && IsDesertRoom(s.CurrentRoom.Id))
        {
            s.HorseThirst++;
        }

        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Location > 0)
            {
                int burning = s.RoomBurning[s.Items[i].Location];
                if (burning == 0 || burning > 1)
                {
                    continue;
                }
                else
                {
                    s.Items[i].Location = 0;
                    IO.OutPrintf(s, "🔥 The fire destroys {0}.\n", s.Items[i].Description);
                }
            }
        }

        for (int i = 1; i <= Constants.MaxRooms; i++)
        {
            if (s.RoomBurning[i] > 0)
            {
                s.RoomBurning[i]--;
            }
        }

        if (s.SnakeRoom > 0 && s.RoomBurning[s.SnakeRoom] > 0)
        {
            s.SnakeRoom = 0;
            IO.OutPrintln(s, "🔥 The fire drives away the rattlesnake.");
        }
        if (s.SnakeRoom > 0 && rng.Next(100) < 30)
        {
            s.SnakeRoom = 0;
        }

        if (s.Thirst > Constants.ThirstLimit - 5)
        {
            IO.OutPrintln(s);
            IO.OutPrintln(s, "🌵 === Your throat is parched. You need water soon. ===");
        }
        if (s.IsHorseSaddled && s.CurrentRoom != null && IsDesertRoom(s.CurrentRoom.Id) && s.HorseThirst > Constants.HorseThirstLimit - 5)
        {
            IO.OutPrintln(s);
            IO.OutPrintln(s, "🐎 === Your horse is showing signs of exhaustion. It needs water soon. ===");
        }

        if (s.Thirst >= Constants.ThirstLimit)
        {
            IO.OutPrintln(s);
            IO.WrapWriteLn(s, "💀 You have collapsed from dehydration. GAME OVER.");
            s.IsPlaying = false;
        }

        if (s.CurrentRoom != null && IsDesertRoom(s.CurrentRoom.Id) && !s.IsRiding)
        {
            IO.OutPrintln(s);
            IO.WrapWriteLn(s, "🔥 The desert heat is overwhelming on foot. You collapse into the sand. GAME OVER.");
            s.IsPlaying = false;
        }

        if (s.IsHorseSaddled && s.CurrentRoom != null && IsDesertRoom(s.CurrentRoom.Id) && s.HorseThirst >= Constants.HorseThirstLimit)
        {
            IO.OutPrintln(s);
            IO.WrapWriteLn(s, "💀 Your horse collapses from dehydration. You are stranded in the desert. GAME OVER.");
            s.IsPlaying = false;
        }

        if (s.Turns == Constants.TwilightTurn)
        {
            IO.OutPrintln(s, "🌇 The sun is getting low.");
        }
        if (s.Turns == Constants.DarkTurn)
        {
            IO.OutPrintln(s, "🌑 It is now dark.");
        }
    }

    public static bool CheckHazards(GameState s, string verb)
    {
        if (s.CurrentRoom != null && s.SnakeRoom == s.CurrentRoom.Id && !SafeVerbs.Contains(verb) && verb != "FREEZE" && verb != "WAIT")
        {
            IO.OutPrintln(s);
            IO.WrapWriteLn(s, "🐍 As you reach out, the rattlesnake strikes! You feel a sharp pain in your hand.");
            IO.OutPrintln(s);
            IO.WrapWriteLn(s, "💀 The venom works quickly. GAME OVER.");
            s.IsPlaying = false;
            return false;
        }
        if (s.CurrentRoom != null && s.OutlawRoom == s.CurrentRoom.Id && !SafeVerbs.Contains(verb) && verb != "SHOOT" && verb != "KILL")
        {
            IO.OutPrintln(s);
            IO.WrapWriteLn(s, "🤠 The outlaw doesn't like you poking around. He draws his gun and fires.");
            IO.OutPrintln(s);
            IO.WrapWriteLn(s, "💥 Everything goes dark. GAME OVER.");
            s.IsPlaying = false;
            return false;
        }
        return true;
    }

    public static void ProcessCommand(GameState s, string cmd, Random rng)
    {
        var (verb, noun) = SplitCommand(cmd);
        bool consumeTurn = true;

        if (!CheckHazards(s, verb))
        {
            return;
        }

        switch (verb)
        {
            case "N": case "NORTH":
                if (s.CurrentRoom?.North != null) PrintMovement(s, "NORTH", s.IsRiding, rng);
                MoveTo(s, s.CurrentRoom?.North, rng);
                break;
            case "S": case "SOUTH":
                if (s.CurrentRoom?.South != null) PrintMovement(s, "SOUTH", s.IsRiding, rng);
                MoveTo(s, s.CurrentRoom?.South, rng);
                break;
            case "E": case "EAST":
                if (s.CurrentRoom?.East != null) PrintMovement(s, "EAST", s.IsRiding, rng);
                MoveTo(s, s.CurrentRoom?.East, rng);
                break;
            case "W": case "WEST":
                if (s.CurrentRoom?.West != null) PrintMovement(s, "WEST", s.IsRiding, rng);
                MoveTo(s, s.CurrentRoom?.West, rng);
                break;
            case "LOOK": case "L":
                CmdExamineItem(s, noun, ref consumeTurn);
                consumeTurn = false;
                break;
            case "EXAMINE": case "X":
                CmdExamineItem(s, noun, ref consumeTurn);
                consumeTurn = false;
                break;
            case "SEARCH":
                IO.Look(s);
                consumeTurn = false;
                break;
            case "HELP": case "?": case "H":
                CmdShowHelp(s, ref consumeTurn);
                break;
            case "INVENTORY": case "I": case "INV":
                CmdHandleInventory(s, ref consumeTurn);
                break;
            case "DRINK":
                CmdDrink(s, ref consumeTurn);
                break;
            case "FILL":
                CmdFillCanteen(s, ref consumeTurn);
                break;
            case "WATER":
                CmdWaterHorse(s, noun, ref consumeTurn);
                break;
            case "LIGHT":
                CmdLightLamp(s, noun, ref consumeTurn);
                break;
            case "FIX":
                CmdFixSomething(s, noun, ref consumeTurn);
                break;
            case "SADDLE":
                CmdSaddleHorse(s, noun, ref consumeTurn);
                break;
            case "PUT":
                if (noun.ToUpperInvariant().Contains("SADDLE"))
                {
                    CmdSaddleHorse(s, "HORSE", ref consumeTurn);
                }
                break;
            case "CLIMB":
                CmdHandleClimb(s, ref consumeTurn, rng);
                break;
            case "SAVE":
                CmdHandleSave(s, noun, ref consumeTurn);
                break;
            case "LOAD":
                CmdHandleLoad(s, noun, ref consumeTurn);
                break;
            case "DROP": case "D":
                CmdHandleDrop(s, noun, ref consumeTurn);
                break;
            case "BURN":
                CmdHandleBurn(s, noun, ref consumeTurn);
                break;
            case "FIRE":
                CmdHandleFire(s, noun, ref consumeTurn);
                break;
            case "MOUNT": case "RIDE":
                CmdHandleMount(s, noun, ref consumeTurn);
                break;
            case "DISMOUNT":
                CmdHandleDismount(s, noun, ref consumeTurn);
                break;
            case "OPEN":
                CmdHandleOpen(s, noun, ref consumeTurn);
                break;
            case "SHOOT": case "KILL":
                CmdHandleShoot(s, noun, ref consumeTurn);
                break;
            case "FREEZE": case "WAIT":
                CmdHandleFreeze(s, noun, ref consumeTurn, rng);
                break;
            case "CHECK":
                if (noun.Equals("INVENTORY", StringComparison.OrdinalIgnoreCase) || noun.Equals("INV", StringComparison.OrdinalIgnoreCase) || noun.Equals("I", StringComparison.OrdinalIgnoreCase))
                {
                    CmdHandleInventory(s, ref consumeTurn);
                }
                break;
            case "SCORE":
                CmdHandleScore(s, ref consumeTurn);
                break;
            case "TAKE": case "GET":
                CmdHandleTake(s, noun, ref consumeTurn);
                break;
            case "QUIT": case "Q":
                s.IsPlaying = false;
                break;
            default:
                IO.OutPrintln(s, "🤷 I don't know how to do that.");
                break;
        }

        if (s.IsPlaying && consumeTurn)
        {
            UpdateWorld(s, rng);
            Persistence.CheckAutosave(s);
        }
    }

    private static void CmdDrink(GameState s, ref bool consumeTurn)
    {
        if (FindItem("CANTEEN", Constants.InvLocation, s) == 0)
        {
            IO.OutPrintln(s, "You don't have anything to drink from.");
        }
        else if (!s.HasWater)
        {
            IO.OutPrintln(s, "Your canteen is empty.");
        }
        else
        {
            s.Thirst = 0;
            if (s.CanteenDrinks > 0) s.CanteenDrinks--;
            if (s.CanteenDrinks <= 0) s.HasWater = false;
            IO.WrapWriteLn(s, "💧 The water is warm but refreshing.");
            IO.OutPrintln(s, "Your thirst is quenched.");
        }
    }

    private static void CmdFillCanteen(GameState s, ref bool consumeTurn)
    {
        if (FindItem("CANTEEN", Constants.InvLocation, s) == 0)
        {
            IO.OutPrintln(s, "You have nothing to fill.");
        }
        else if (s.CurrentRoom?.Id == 3 && s.IsPumpFixed)
        {
            s.HasWater = true;
            s.CanteenDrinks = 3;
            IO.OutPrintln(s, "💧 You fill your canteen with fresh water from the pump.");
            if (!s.ScoredFirstFill)
            {
                s.ScoredFirstFill = true;
                s.Score += Constants.ScoreFirstFill;
            }
        }
        else if (s.CurrentRoom?.Id == Constants.StreamRoomID)
        {
            s.HasWater = true;
            s.CanteenDrinks = 3;
            IO.OutPrintln(s, "💧 You fill your canteen with cold stream water.");
        }
        else
        {
            IO.OutPrintln(s, "There is no water here.");
        }
    }

    private static void CmdLightLamp(GameState s, string noun, ref bool consumeTurn)
    {
        string nounUpper = noun.Trim().ToUpperInvariant();
        if (!string.IsNullOrEmpty(noun) && nounUpper != "MATCH" && nounUpper != "MATCHES")
        {
            IO.OutPrintln(s, "Light what?");
            return;
        }
        if (FindItem("LAMP", Constants.InvLocation, s) > 0)
        {
            s.IsLampLit = true;
            IO.WrapWriteLn(s, "🔦 You light the lamp. A yellow glow illuminates the room.");
            if (!s.ScoredLampLight)
            {
                s.ScoredLampLight = true;
                s.Score += Constants.ScoreLampLight;
            }
        }
        else
        {
            s.TempLightTurns = 3;
            IO.WrapWriteLn(s, "🔥 You strike a match. The room brightens for a moment.");
        }
    }

    private static void CmdShowHelp(GameState s, ref bool consumeTurn)
    {
        IO.OutPrintln(s);
        IO.OutPrintln(s, "Available Commands:");
        IO.OutPrintln(s, "  🚶 N, S, E, W      - Move North, South, East, West");
        IO.OutPrintln(s, "  👀 LOOK (L)        - Look around");
        IO.OutPrintln(s, "  🔍 EXAMINE (X)     - Look closely at an item");
        IO.OutPrintln(s, "  🖐️  TAKE (GET)      - Pick up an item");
        IO.OutPrintln(s, "  ✋  DROP            - Leave an item");
        IO.OutPrintln(s, "  🎒 INVENTORY (I)   - Check your gear");
        IO.OutPrintln(s, "  💧 DRINK           - Drink from your canteen");
        IO.OutPrintln(s, "  🚰 FILL            - Fill canteen at a water source");
        IO.OutPrintln(s, "  🐎 WATER           - Water your horse at a water source");
        IO.OutPrintln(s, "  🔦 LIGHT           - Light your lamp if you have matches");
        IO.OutPrintln(s, "  🔧 FIX             - Repair something");
        IO.OutPrintln(s, "  🏇 SADDLE          - Put a saddle on the horse");
        IO.OutPrintln(s, "  ❄️  FREEZE (WAIT)   - Stay still to avoid danger");
        IO.OutPrintln(s, "  🔥 BURN            - Burn a flammable item (requires matches)");
        IO.OutPrintln(s, "  🔥 FIRE            - Start a fire in certain rooms (requires matches)");
        IO.OutPrintln(s, "  🧗 CLIMB           - Climb a steep obstacle");
        IO.OutPrintln(s, "  💾 SAVE / LOAD     - Save or load your progress");
        IO.OutPrintln(s, "  🏆 SCORE           - Show current score");
        IO.OutPrintln(s, "  ❓ HELP (H)        - Show this list");
        IO.OutPrintln(s, "  🚪 QUIT (Q)        - Exit");
        IO.OutPrintln(s);
        consumeTurn = false;
    }

    private static void CmdExamineItem(GameState s, string targetNoun, ref bool consumeTurn)
    {
        string noun = targetNoun.Trim();
        if (noun.StartsWith("AT ", StringComparison.OrdinalIgnoreCase))
        {
            noun = noun.Substring(3).Trim();
        }

        if (s.CurrentRoom == null) return;

        int itemID = FindItem(noun, Constants.InvLocation, s);
        if (itemID == 0)
        {
            itemID = FindItem(noun, s.CurrentRoom.Id, s);
        }

        if (itemID > 0)
        {
            IO.WrapWriteLn(s, s.Items[itemID].Details);
            if (s.Items[itemID].Name == "ROCK")
            {
                int keyId = FindItemAny("KEY", s);
                if (keyId > 0 && s.Items[keyId].Location == 0)
                {
                    s.Items[keyId].Location = s.CurrentRoom.Id;
                    IO.OutPrintln(s);
                    IO.OutPrintln(s, "You lift the rock. A small brass key is hidden beneath it.");
                }
            }
            if (s.Items[itemID].Name == "BOOK")
            {
                int noteId = FindItemAny("NOTE", s);
                if (noteId > 0 && s.Items[noteId].Location == 0)
                {
                    s.Items[noteId].Location = Constants.InvLocation;
                    IO.OutPrintln(s);
                    IO.OutPrintln(s, "A small folded note falls out of the book.");
                    if (!s.ScoredNoteFound)
                    {
                        s.ScoredNoteFound = true;
                        s.Score += Constants.ScoreNoteFound;
                    }
                }
            }
        }
        else if (string.IsNullOrEmpty(noun))
        {
            IO.Look(s);
        }
        else
        {
            IO.OutPrintln(s, "You don't see that here.");
        }
        consumeTurn = false;
    }

    private static void CmdFixSomething(GameState s, string targetNoun, ref bool consumeTurn)
    {
        string noun = targetNoun.Trim().ToUpperInvariant();
        if (noun == "PUMP" && s.CurrentRoom?.Id == 3)
        {
            if (FindItem("LEATHER", Constants.InvLocation, s) > 0)
            {
                s.IsPumpFixed = true;
                IO.OutPrintln(s, "You fix the pump. Water starts to flow.");
                s.Items[3].Description = "a working water pump";
                if (!s.ScoredPumpFix)
                {
                    s.ScoredPumpFix = true;
                    s.Score += Constants.ScorePumpFix;
                }
            }
            else
            {
                IO.OutPrintln(s, "You need leather.");
            }
        }
        else if ((noun == "WIRE" || noun == "WIRES" || noun == "TELEGRAPH") && s.CurrentRoom?.Id == 2)
        {
            if (s.IsTelegraphFixed)
            {
                IO.OutPrintln(s, "The telegraph is already repaired.");
            }
            else if (FindItem("WIRE", Constants.InvLocation, s) > 0)
            {
                s.IsTelegraphFixed = true;
                IO.OutPrintln(s, "You splice the copper wire and restore the telegraph line.");
                if (s.RoomRegistry[2] != null)
                {
                    s.RoomRegistry[2]!.Description = "The telegraph has been repaired. The line hums faintly with life.";
                }
                if (!s.ScoredTelegraphFix)
                {
                    s.ScoredTelegraphFix = true;
                    s.Score += Constants.ScoreTelegraphFix;
                }
                s.Items[4].Location = 0;
            }
            else
            {
                IO.OutPrintln(s, "You need copper wire.");
            }
        }
        else
        {
            IO.OutPrintln(s, "Nothing to fix here.");
        }
    }

    private static void CmdWaterHorse(GameState s, string targetNoun, ref bool consumeTurn)
    {
        string noun = targetNoun.Trim().ToUpperInvariant();
        if (!string.IsNullOrEmpty(noun) && noun != "HORSE" && noun != "MARE")
        {
            IO.OutPrintln(s, "Water what?");
            return;
        }
        if (!s.IsHorseSaddled)
        {
            IO.OutPrintln(s, "You don't have a horse with you.");
            return;
        }
        if (s.CurrentRoom?.Id != Constants.StreamRoomID)
        {
            IO.OutPrintln(s, "There is no water here for your horse.");
            return;
        }
        s.HorseThirst = 0;
        IO.OutPrintln(s, "Your horse drinks deeply from the stream.");
    }

    private static void CmdSaddleHorse(GameState s, string targetNoun, ref bool consumeTurn)
    {
        string noun = targetNoun.Trim().ToUpperInvariant();
        if (!string.IsNullOrEmpty(noun) && noun != "HORSE" && noun != "ON HORSE" && noun != "MARE")
        {
            IO.OutPrintln(s, "Saddle what?");
            return;
        }
        if (s.CurrentRoom == null) return;

        int horseID = FindItem("HORSE", s.CurrentRoom.Id, s);
        if (horseID == 0)
        {
            IO.OutPrintln(s, "There is no horse here.");
            return;
        }
        int saddleID = FindItem("SADDLE", Constants.InvLocation, s);
        if (saddleID == 0)
        {
            IO.OutPrintln(s, "You need a saddle.");
            return;
        }
        if (s.IsHorseSaddled)
        {
            IO.OutPrintln(s, "The horse is already saddled.");
            return;
        }
        s.IsHorseSaddled = true;
        s.Items[saddleID].Location = 0;
        s.Items[horseID].Description = "a saddled horse";
        s.Items[horseID].Details = "A calm, saddle-ready horse. It looks steady and patient.";
        IO.OutPrintln(s, "You secure the saddle onto the horse. It stands quietly.");
    }

    private static void CmdHandleMount(GameState s, string noun, ref bool consumeTurn)
    {
        string nounUpper = noun.Trim().ToUpperInvariant();
        if (!string.IsNullOrEmpty(nounUpper) && nounUpper != "HORSE" && nounUpper != "MARE")
        {
            IO.OutPrintln(s, "Mount what?");
            return;
        }
        if (s.CurrentRoom == null) return;

        if (s.IsRiding)
        {
            IO.OutPrintln(s, "You are already riding.");
        }
        else if (FindItem("HORSE", s.CurrentRoom.Id, s) > 0)
        {
            if (s.IsHorseSaddled)
            {
                s.IsRiding = true;
                int horseID = FindItem("HORSE", s.CurrentRoom.Id, s);
                s.Items[horseID].Location = Constants.InvLocation;
                IO.OutPrintln(s, "You swing yourself into the saddle. You are now riding.");
            }
            else
            {
                IO.OutPrintln(s, "The horse needs a saddle before you can ride her.");
            }
        }
        else
        {
            IO.OutPrintln(s, "There is no horse here.");
        }
    }

    private static void CmdHandleDismount(GameState s, string noun, ref bool consumeTurn)
    {
        string nounUpper = noun.Trim().ToUpperInvariant();
        if (!string.IsNullOrEmpty(nounUpper) && nounUpper != "HORSE" && nounUpper != "MARE")
        {
            IO.OutPrintln(s, "Dismount what?");
            return;
        }
        if (!s.IsRiding)
        {
            IO.OutPrintln(s, "You aren't riding anything.");
        }
        else
        {
            s.IsRiding = false;
            int horseID = FindItem("HORSE", Constants.InvLocation, s);
            if (horseID > 0 && s.CurrentRoom != null)
            {
                s.Items[horseID].Location = s.CurrentRoom.Id;
            }
            IO.OutPrintln(s, "You dismount and stand beside your horse.");
        }
    }

    private static void CmdHandleOpen(GameState s, string noun, ref bool consumeTurn)
    {
        string nounUpper = noun.Trim().ToUpperInvariant();
        if (nounUpper == "BOX" && s.CurrentRoom?.Id == 7)
        {
            if (s.IsBoxOpen)
            {
                IO.OutPrintln(s, "It is already open.");
            }
            else if (FindItem("KEY", Constants.InvLocation, s) == 0)
            {
                IO.OutPrintln(s, "The box is locked. You need a key.");
            }
            else
            {
                s.IsBoxOpen = true;
                s.Items[8].Location = 7;
                IO.OutPrintln(s, "You unlock the box. Inside lies a heavy revolver.");
                if (!s.ScoredBoxOpen)
                {
                    s.ScoredBoxOpen = true;
                    s.Score += Constants.ScoreBoxOpen;
                }
            }
        }
        else
        {
            IO.OutPrintln(s, "There is nothing to open here.");
            consumeTurn = false;
        }
    }

    private static void CmdHandleShoot(GameState s, string noun, ref bool consumeTurn)
    {
        if (FindItem("REVOLVER", Constants.InvLocation, s) == 0)
        {
            IO.OutPrintln(s, "You have nothing to shoot with.");
        }
        else if (s.CurrentRoom != null && s.OutlawRoom == s.CurrentRoom.Id)
        {
            s.OutlawRoom = 0;
            IO.WrapWriteLn(s, "💥 You draw your revolver and fire first. The outlaw falls to the ground.");
            IO.OutPrintln(s, "💀 The threat is gone.");
            if (!s.ScoredOutlawKill)
            {
                s.ScoredOutlawKill = true;
                s.Score += Constants.ScoreOutlawKill;
            }
        }
        else
        {
            IO.OutPrintln(s, "Nothing here to shoot.");
        }
    }

    private static void CmdHandleFreeze(GameState s, string noun, ref bool consumeTurn, Random rng)
    {
        IO.OutPrintln(s, "You stay perfectly still. The snake watches you...");
        if (rng.Next(100) < 50)
        {
            s.SnakeRoom = 0;
            IO.OutPrintln(s, "The snake loses interest and slithers into the shadows.");
        }
    }

    private static void CmdHandleInventory(GameState s, ref bool consumeTurn)
    {
        IO.OutPrintln(s, "You are carrying:");
        for (int i = 1; i <= Constants.MaxItems; i++)
        {
            if (s.Items[i].Location == Constants.InvLocation)
            {
                IO.OutPrintf(s, "  - {0}\n", s.Items[i].Description);
            }
        }
        consumeTurn = false;
    }

    private static void CmdHandleScore(GameState s, ref bool consumeTurn)
    {
        IO.OutPrintf(s, "🏆 Score: {0}\n", s.Score);
        consumeTurn = false;
    }

    private static void CmdHandleSave(GameState s, string noun, ref bool consumeTurn)
    {
        string path = "data/save.db";
        if (!string.IsNullOrWhiteSpace(noun))
        {
            path = "data/" + noun.Trim().ToLowerInvariant();
            if (!path.EndsWith(".db")) path += ".db";
        }
        Persistence.SaveGame(s, path);
        consumeTurn = false;
    }

    private static void CmdHandleLoad(GameState s, string noun, ref bool consumeTurn)
    {
        string path = "data/save.db";
        if (!string.IsNullOrWhiteSpace(noun))
        {
            path = "data/" + noun.Trim().ToLowerInvariant();
            if (!path.EndsWith(".db")) path += ".db";
        }
        Persistence.LoadGame(s, path);
        consumeTurn = false;
    }

    private static void CmdHandleTake(GameState s, string noun, ref bool consumeTurn)
    {
        if (s.CurrentRoom == null) return;

        int itemID = FindItem(noun, s.CurrentRoom.Id, s);
        if (itemID > 0)
        {
            int carryCount = 0;
            for (int i = 1; i <= Constants.MaxItems; i++)
            {
                if (s.Items[i].Location == Constants.InvLocation && s.Items[i].IsTakeable)
                {
                    carryCount++;
                }
            }
            if (carryCount >= Constants.MaxCarry)
            {
                IO.OutPrintln(s, "You can't carry any more. Drop something first.");
                return;
            }
            if (!s.Items[itemID].IsTakeable)
            {
                switch (s.Items[itemID].Name)
                {
                    case "PUMP": IO.OutPrintln(s, "The pump is fixed in place."); break;
                    case "HORSE": IO.OutPrintln(s, "It's too big to carry."); break;
                    case "BOX": IO.OutPrintln(s, "It's bolted down."); break;
                    case "ROCK": IO.OutPrintln(s, "It's too heavy to carry."); break;
                    default: IO.OutPrintln(s, "You can't take that."); break;
                }
                return;
            }
            s.Items[itemID].Location = Constants.InvLocation;
            IO.OutPrintf(s, "🎒 Taken: {0}.\n", s.Items[itemID].Description);
            if (!s.ItemScored[itemID])
            {
                s.ItemScored[itemID] = true;
                s.Score += Constants.ScoreItemPickup;
            }
        }
        else
        {
            IO.OutPrintln(s, "Not here.");
        }
    }

    private static void CmdHandleClimb(GameState s, ref bool consumeTurn, Random rng)
    {
        if (s.CurrentRoom?.Id == 12)
        {
            MoveTo(s, s.RoomRegistry[Constants.StreamRoomID], rng);
        }
        else
        {
            IO.OutPrintln(s, "There is nothing to climb here.");
        }
    }

    private static void CmdHandleDrop(GameState s, string noun, ref bool consumeTurn)
    {
        if (s.CurrentRoom == null) return;

        int itemID = FindItem(noun, Constants.InvLocation, s);
        if (itemID > 0)
        {
            s.Items[itemID].Location = s.CurrentRoom.Id;
            IO.OutPrintf(s, "✋ Dropped: {0}.\n", s.Items[itemID].Description);
        }
        else
        {
            IO.OutPrintln(s, "You aren't carrying that.");
        }
    }

    private static void CmdHandleBurn(GameState s, string noun, ref bool consumeTurn)
    {
        string target = noun.Trim().ToUpperInvariant();
        if (string.IsNullOrEmpty(target))
        {
            IO.OutPrintln(s, "Burn what?");
            return;
        }
        if (FindItem("MATCHES", Constants.InvLocation, s) == 0)
        {
            IO.OutPrintln(s, "You have nothing to burn it with.");
            return;
        }
        if (s.CurrentRoom == null) return;

        int itemID = FindItem(target, Constants.InvLocation, s);
        if (itemID == 0)
        {
            itemID = FindItem(target, s.CurrentRoom.Id, s);
        }
        if (itemID == 0)
        {
            IO.OutPrintln(s, "You don't see that here.");
            return;
        }
        string name = s.Items[itemID].Name;
        if (name != "BOOK" && name != "LEDGER" && name != "LEATHER" && name != "MAP" && name != "SADDLE")
        {
            IO.OutPrintln(s, "It doesn't burn.");
            return;
        }
        s.Items[itemID].Location = 0;
        IO.OutPrintln(s, "You burn it to ash.");
    }

    private static void CmdHandleFire(GameState s, string noun, ref bool consumeTurn)
    {
        if (FindItem("MATCHES", Constants.InvLocation, s) == 0)
        {
            IO.OutPrintln(s, "You have nothing to start a fire with.");
            return;
        }
        if (s.CurrentRoom == null) return;

        int id = s.CurrentRoom.Id;
        if (id != 2 && id != 3 && id != 5)
        {
            IO.OutPrintln(s, "There is nothing here that will catch fire.");
            return;
        }
        if (s.RoomBurning[id] > 0)
        {
            IO.OutPrintln(s, "A fire is already burning here.");
            return;
        }
        s.RoomBurning[id] = 3;
        IO.OutPrintln(s, "🔥 You start a fire. The room glows with heat.");
        if (s.SnakeRoom == id)
        {
            s.SnakeRoom = 0;
            IO.OutPrintln(s, "🔥 The rattlesnake recoils from the flames and disappears.");
        }
    }
}
