unit u_commands;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, u_types, u_state;

procedure ProcessCommand(var S: TGameState; const Cmd: string);

implementation

uses
  u_io, u_persistence;

type
  TCommandHandler = procedure(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);

  TCommandEntry = record
    Verb: string;
    Handler: TCommandHandler;
  end;

procedure SplitCommand(const Cmd: string; var V, N: string);
var
  SpacePos: Integer;
  Trimmed: string;
begin
  V := '';
  N := '';
  Trimmed := Trim(Cmd);
  SpacePos := Pos(' ', Trimmed);
  if SpacePos > 0 then begin
    V := UpperCase(Copy(Trimmed, 1, SpacePos - 1));
    N := Trim(Copy(Trimmed, SpacePos + 1, Length(Trimmed)));
  end else
    V := UpperCase(Trimmed);
end;

function FindItem(const ItemName: string; Loc: Integer; const S: TGameState): Integer;
var
  i: Integer;
  UpperName: string;
begin
  Result := 0;
  UpperName := UpperCase(ItemName);
  for i := 1 to MAX_ITEMS do
    if (S.Items[i].Location = Loc) and (S.Items[i].Name = UpperName) then begin
      Result := i;
      Exit;
    end;
end;

function FindItemAny(const ItemName: string; const S: TGameState): Integer;
var
  i: Integer;
  UpperName: string;
begin
  Result := 0;
  UpperName := UpperCase(ItemName);
  for i := 1 to MAX_ITEMS do
    if S.Items[i].Name = UpperName then begin
      Result := i;
      Exit;
    end;
end;

function IsDesertRoom(RoomID: Integer): Boolean;
begin
  Result := (RoomID >= 8) and (RoomID <= 13);
end;

procedure PrintMovement(const Direction: string; IsRiding: Boolean);
var
  Idx: Integer;
  Msg: string;
  Emoji: string;
begin
  Idx := Random(5);
  if IsRiding then begin
    Emoji := '🏇 ';
    case Idx of
      0: Msg := 'You ride ' + Direction + '.';
      1: Msg := 'You guide your horse ' + Direction + '.';
      2: Msg := 'You trot ' + Direction + '.';
      3: Msg := 'You and your mare head ' + Direction + '.';
      4: Msg := 'The horse carries you ' + Direction + '.';
    end;
  end else begin
    Emoji := '🚶 ';
    case Idx of
      0: Msg := 'You walk ' + Direction + '.';
      1: Msg := 'You trek ' + Direction + ' through the dust.';
      2: Msg := 'You head ' + Direction + '.';
      3: Msg := 'You make your way ' + Direction + '.';
      4: Msg := 'You trudge ' + Direction + ' across the dry ground.';
    end;
  end;
  WriteLn(Emoji, Msg);
end;

procedure MoveTo(var S: TGameState; NewRoom: PRoom);
var
  i: Integer;
begin
  if NewRoom = nil then
    WriteLn('You cannot go that way.')
  else if S.IsRiding and ((NewRoom^.ID = 2) or (NewRoom^.ID = 4) or (NewRoom^.ID = 5) or (NewRoom^.ID = 7)) then
    WriteLn('You can''t bring a horse in there. Dismount first.')
  else if (S.CurrentRoom^.ID = 6) and (NewRoom^.ID = DESERT_ENTRY_ROOM_ID) and (not S.IsRiding) then
    WriteLn('The desert is too dangerous on foot. You must be riding a saddled horse.')
  else begin
    S.CurrentRoom := NewRoom;
    if S.IsRiding then begin
      for i := 1 to MAX_ITEMS do
        if S.Items[i].Name = 'HORSE' then begin
          S.Items[i].Location := INV_LOCATION;
          Break;
        end;
    end;
    if (S.CurrentRoom^.ID <> 1) and (not S.RoomVisited[S.CurrentRoom^.ID]) then begin
      S.RoomVisited[S.CurrentRoom^.ID] := True;
      Inc(S.Score, SCORE_ROOM_VISIT);
    end;
    if (S.CurrentRoom^.ID <> 1) and (Random(100) < 20) then
      S.SnakeRoom := S.CurrentRoom^.ID
    else
      S.SnakeRoom := 0;

    if (S.CurrentRoom^.ID <> 1) and (S.CurrentRoom^.ID <> 7) and (Random(100) < 15) then
      S.OutlawRoom := S.CurrentRoom^.ID
    else
      S.OutlawRoom := 0;
    Look(S);
  end;
end;

procedure UpdateWorld(var S: TGameState);
var
  i: Integer;
begin
  Inc(S.Turns);
  Inc(S.Thirst);
  if (S.TempLightTurns > 0) and (not S.IsLampLit) then Dec(S.TempLightTurns);
  if S.IsHorseSaddled and IsDesertRoom(S.CurrentRoom^.ID) then Inc(S.HorseThirst);
  for i := 1 to MAX_ITEMS do
    if S.Items[i].Location > 0 then
      if S.RoomBurning[S.Items[i].Location] = 0 then
        continue
      else if S.RoomBurning[S.Items[i].Location] > 1 then
        continue
      else begin
        S.Items[i].Location := 0;
        WriteLn('🔥 The fire destroys ', S.Items[i].Description, '.');
      end;

  for i := 1 to MAX_ROOMS do
    if S.RoomBurning[i] > 0 then
      Dec(S.RoomBurning[i]);

  if (S.SnakeRoom > 0) and (S.RoomBurning[S.SnakeRoom] > 0) then begin
    S.SnakeRoom := 0;
    WriteLn('🔥 The fire drives away the rattlesnake.');
  end;
  if (S.SnakeRoom > 0) and (Random(100) < 30) then S.SnakeRoom := 0;
  if S.Thirst > THIRST_LIMIT - 5 then begin
    WriteLn;
    WriteLn('🌵 === Your throat is parched. You need water soon. ===');
  end;
  if S.IsHorseSaddled and IsDesertRoom(S.CurrentRoom^.ID) and (S.HorseThirst > HORSE_THIRST_LIMIT - 5) then begin
    WriteLn;
    WriteLn('🐎 === Your horse is showing signs of exhaustion. It needs water soon. ===');
  end;

  if S.Thirst >= THIRST_LIMIT then begin
    WriteLn;
    WrapWriteLn('💀 You have collapsed from dehydration. GAME OVER.');
    S.IsPlaying := False;
  end;

  if IsDesertRoom(S.CurrentRoom^.ID) and (not S.IsRiding) then begin
    WriteLn;
    WrapWriteLn('🔥 The desert heat is overwhelming on foot. You collapse into the sand. GAME OVER.');
    S.IsPlaying := False;
  end;

  if S.IsHorseSaddled and IsDesertRoom(S.CurrentRoom^.ID) and (S.HorseThirst >= HORSE_THIRST_LIMIT) then begin
    WriteLn;
    WrapWriteLn('💀 Your horse collapses from dehydration. You are stranded in the desert. GAME OVER.');
    S.IsPlaying := False;
  end;

  if S.Turns = TWILIGHT_TURN then WriteLn('🌇 The sun is getting low.');
  if S.Turns = DARK_TURN then WriteLn('🌑 It is now dark.');
end;

procedure Drink(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if FindItem('CANTEEN', INV_LOCATION, S) = 0 then
    WriteLn('You don''t have anything to drink from.')
  else if not S.HasWater then
    WriteLn('Your canteen is empty.')
  else begin
    S.Thirst := 0;
    if S.CanteenDrinks > 0 then Dec(S.CanteenDrinks);
    if S.CanteenDrinks <= 0 then S.HasWater := False;
    WrapWriteLn('💧 The water is warm but refreshing.');
    WriteLn('Your thirst is quenched.');
  end;
end;

procedure FillCanteen(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if FindItem('CANTEEN', INV_LOCATION, S) = 0 then
    WriteLn('You have nothing to fill.')
  else if (S.CurrentRoom^.ID = 3) and S.IsPumpFixed then begin
    S.HasWater := True;
    S.CanteenDrinks := 3;
    WriteLn('💧 You fill your canteen with fresh water from the pump.');
    if not S.ScoredFirstFill then begin
      S.ScoredFirstFill := True;
      Inc(S.Score, SCORE_FIRST_FILL);
    end;
  end else if (S.CurrentRoom^.ID = STREAM_ROOM_ID) then begin
    S.HasWater := True;
    S.CanteenDrinks := 3;
    WriteLn('💧 You fill your canteen with cold stream water.');
  end else
    WriteLn('There is no water here.');
end;

procedure LightLamp(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if (Noun <> '') and (UpperCase(Trim(Noun)) <> 'MATCH') and (UpperCase(Trim(Noun)) <> 'MATCHES') then begin
    WriteLn('Light what?');
    Exit;
  end;
  if FindItem('LAMP', INV_LOCATION, S) > 0 then begin
    S.IsLampLit := True;
    WrapWriteLn('🔦 You light the lamp. A yellow glow illuminates the room.');
    if not S.ScoredLampLight then begin
      S.ScoredLampLight := True;
      Inc(S.Score, SCORE_LAMP_LIGHT);
    end;
  end else begin
    S.TempLightTurns := 3;
    WrapWriteLn('🔥 You strike a match. The room brightens for a moment.');
  end;
end;

procedure ShowHelp(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  WriteLn;
  WriteLn('Available Commands:');
  WriteLn('  🚶 N, S, E, W      - Move North, South, East, West');
  WriteLn('  👀 LOOK (L)        - Look around');
  WriteLn('  🔍 EXAMINE (X)     - Look closely at an item');
  WriteLn('  🖐️  TAKE (GET)      - Pick up an item');
  WriteLn('  ✋  DROP            - Leave an item');
  WriteLn('  🎒 INVENTORY (I)   - Check your gear');
  WriteLn('  💧 DRINK           - Drink from your canteen');
  WriteLn('  🚰 FILL            - Fill canteen at a water source');
  WriteLn('  🐎 WATER           - Water your horse at a water source');
  WriteLn('  🔦 LIGHT           - Light your lamp if you have matches');
  WriteLn('  🔧 FIX             - Repair something');
  WriteLn('  🏇 SADDLE          - Put a saddle on the horse');
  WriteLn('  ❄️  FREEZE (WAIT)   - Stay still to avoid danger');
  WriteLn('  🔥 BURN            - Burn a flammable item (requires matches)');
  WriteLn('  🔥 FIRE            - Start a fire in certain rooms (requires matches)');
  WriteLn('  🧗 CLIMB           - Climb a steep obstacle');
  WriteLn('  💾 SAVE / LOAD     - Save or load your progress');
  WriteLn('  🏆 SCORE           - Show current score');
  WriteLn('  ❓ HELP (H)        - Show this list');
  WriteLn('  🚪 QUIT (Q)        - Exit');
  WriteLn;
  ConsumeTurn := False;
end;

procedure ExamineItem(var S: TGameState; const TargetNoun: string; var ConsumeTurn: Boolean);
var
  ItemID: Integer;
  KeyId: Integer;
  NoteId: Integer;
  Noun: string;
begin
  Noun := TargetNoun;
  if (Copy(UpperCase(Noun), 1, 3) = 'AT ') then Noun := Trim(Copy(Noun, 4, Length(Noun)));
  ItemID := FindItem(Noun, INV_LOCATION, S);
  if ItemID = 0 then ItemID := FindItem(Noun, S.CurrentRoom^.ID, S);
  if ItemID > 0 then begin
    WrapWriteLn(S.Items[ItemID].Details);
    if S.Items[ItemID].Name = 'ROCK' then begin
      KeyId := FindItemAny('KEY', S);
      if (KeyId > 0) and (S.Items[KeyId].Location = 0) then begin
        S.Items[KeyId].Location := S.CurrentRoom^.ID;
        WriteLn;
        WriteLn('You lift the rock. A small brass key is hidden beneath it.');
      end;
    end;
    if S.Items[ItemID].Name = 'BOOK' then begin
      NoteId := FindItemAny('NOTE', S);
      if (NoteId > 0) and (S.Items[NoteId].Location = 0) then begin
        S.Items[NoteId].Location := INV_LOCATION;
        WriteLn;
        WriteLn('A small folded note falls out of the book.');
        if not S.ScoredNoteFound then begin
          S.ScoredNoteFound := True;
          Inc(S.Score, SCORE_NOTE_FOUND);
        end;
      end;
    end;
  end else if Noun = '' then
    Look(S)
  else
    WriteLn('You don''t see that here.');
  ConsumeTurn := False;
end;

procedure FixSomething(var S: TGameState; const TargetNoun: string; var ConsumeTurn: Boolean);
var
  Noun: string;
begin
  Noun := UpperCase(Trim(TargetNoun));
  if (Noun = 'PUMP') and (S.CurrentRoom^.ID = 3) then begin
    if FindItem('LEATHER', INV_LOCATION, S) > 0 then begin
      S.IsPumpFixed := True;
      WriteLn('You fix the pump. Water starts to flow.');
      S.Items[3].Description := 'a working water pump';
      if not S.ScoredPumpFix then begin
        S.ScoredPumpFix := True;
        Inc(S.Score, SCORE_PUMP_FIX);
      end;
    end else
      WriteLn('You need leather.');
  end else if ((Noun = 'WIRE') or (Noun = 'WIRES') or (Noun = 'TELEGRAPH')) and (S.CurrentRoom^.ID = 2) then begin
    if S.IsTelegraphFixed then begin
      WriteLn('The telegraph is already repaired.');
    end else if FindItem('WIRE', INV_LOCATION, S) > 0 then begin
      S.IsTelegraphFixed := True;
      WriteLn('You splice the copper wire and restore the telegraph line.');
      if S.RoomRegistry[2] <> nil then
        S.RoomRegistry[2]^.Description := 'The telegraph has been repaired. The line hums faintly with life.';
      if not S.ScoredTelegraphFix then begin
        S.ScoredTelegraphFix := True;
        Inc(S.Score, SCORE_TELEGRAPH_FIX);
      end;
      S.Items[4].Location := 0;
    end else
      WriteLn('You need copper wire.');
  end else
    WriteLn('Nothing to fix here.');
end;

procedure WaterHorse(var S: TGameState; const TargetNoun: string; var ConsumeTurn: Boolean);
var
  Noun: string;
begin
  Noun := UpperCase(Trim(TargetNoun));
  if (Noun <> '') and (Noun <> 'HORSE') then begin
    WriteLn('Water what?');
    Exit;
  end;
  if not S.IsHorseSaddled then begin
    WriteLn('You don''t have a horse with you.');
    Exit;
  end;
  if S.CurrentRoom^.ID <> STREAM_ROOM_ID then begin
    WriteLn('There is no water here for your horse.');
    Exit;
  end;
  S.HorseThirst := 0;
  WriteLn('Your horse drinks deeply from the stream.');
end;

procedure SaddleHorse(var S: TGameState; const TargetNoun: string; var ConsumeTurn: Boolean);
var
  HorseID: Integer;
  Noun: string;
begin
  Noun := UpperCase(Trim(TargetNoun));
  if (Noun <> '') and (Noun <> 'HORSE') and (Noun <> 'ON HORSE') then begin
    WriteLn('Saddle what?');
    Exit;
  end;
  HorseID := FindItem('HORSE', S.CurrentRoom^.ID, S);
  if HorseID = 0 then begin
    WriteLn('There is no horse here.');
    Exit;
  end;
  if FindItem('SADDLE', INV_LOCATION, S) = 0 then begin
    WriteLn('You need a saddle.');
    Exit;
  end;
  if S.IsHorseSaddled then begin
    WriteLn('The horse is already saddled.');
    Exit;
  end;
  S.IsHorseSaddled := True;
  S.Items[HorseID].Description := 'a saddled horse';
  S.Items[HorseID].Details := 'A calm, saddle-ready horse. It looks steady and patient.';
  WriteLn('You secure the saddle onto the horse. It stands quietly.');
end;

procedure HandleMount(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if S.IsRiding then WriteLn('You are already riding.')
  else if FindItem('HORSE', S.CurrentRoom^.ID, S) > 0 then begin
    if S.IsHorseSaddled then begin
      S.IsRiding := True;
      S.Items[FindItem('HORSE', S.CurrentRoom^.ID, S)].Location := INV_LOCATION;
      WriteLn('You swing yourself into the saddle. You are now riding.');
    end else
      WriteLn('The horse needs a saddle before you can ride her.');
  end else
    WriteLn('There is no horse here.');
end;

procedure HandleDismount(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if not S.IsRiding then WriteLn('You aren''t riding anything.')
  else begin
    S.IsRiding := False;
    if FindItem('HORSE', INV_LOCATION, S) > 0 then
      S.Items[FindItem('HORSE', INV_LOCATION, S)].Location := S.CurrentRoom^.ID;
    WriteLn('You dismount and stand beside your horse.');
  end;
end;

procedure HandleOpen(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if (Noun = 'BOX') and (S.CurrentRoom^.ID = 7) then begin
    if S.IsBoxOpen then WriteLn('It is already open.')
    else begin
      if FindItem('KEY', INV_LOCATION, S) = 0 then
        WriteLn('The box is locked. You need a key.')
      else begin
        S.IsBoxOpen := True;
        S.Items[8].Location := 7;
        WriteLn('You unlock the box. Inside lies a heavy revolver.');
        if not S.ScoredBoxOpen then begin
          S.ScoredBoxOpen := True;
          Inc(S.Score, SCORE_BOX_OPEN);
        end;
      end;
    end;
  end else begin
    WriteLn('There is nothing to open here.');
    ConsumeTurn := False;
  end;
end;

procedure HandleShoot(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if FindItem('REVOLVER', INV_LOCATION, S) = 0 then
    WriteLn('You have nothing to shoot with.')
  else if S.OutlawRoom = S.CurrentRoom^.ID then begin
    S.OutlawRoom := 0;
    WrapWriteLn('💥 You draw your revolver and fire first. The outlaw falls to the ground.');
    WriteLn('💀 The threat is gone.');
    if not S.ScoredOutlawKill then begin
      S.ScoredOutlawKill := True;
      Inc(S.Score, SCORE_OUTLAW_KILL);
    end;
  end else
    WriteLn('Nothing here to shoot.');
end;

procedure HandleFreeze(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  WriteLn('You stay perfectly still. The snake watches you...');
  if Random(100) < 50 then begin
    S.SnakeRoom := 0;
    WriteLn('The snake loses interest and slithers into the shadows.');
  end;
end;

procedure HandleLook(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  ExamineItem(S, Noun, ConsumeTurn);
  ConsumeTurn := False;
end;

procedure HandleSearch(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  Look(S);
  ConsumeTurn := False;
end;

procedure HandleInventory(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
var
  i: Integer;
begin
  WriteLn('You are carrying:');
  for i := 1 to MAX_ITEMS do if S.Items[i].Location = INV_LOCATION then WriteLn('  - ', S.Items[i].Description);
  ConsumeTurn := False;
end;

procedure HandleScore(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  WriteLn('🏆 Score: ', S.Score);
  ConsumeTurn := False;
end;

procedure HandleSave(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  SaveGame(S, 'data/save.ini');
  ConsumeTurn := False;
end;

procedure HandleLoad(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  LoadGame(S, 'data/save.ini');
  ConsumeTurn := False;
end;

procedure HandleTake(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
var
  ItemId: Integer;
  i: Integer;
  CarryCount: Integer;
begin
  ItemId := FindItem(Noun, S.CurrentRoom^.ID, S);
  if ItemId > 0 then begin
    CarryCount := 0;
    for i := 1 to MAX_ITEMS do
      if (S.Items[i].Location = INV_LOCATION) and S.Items[i].IsTakeable then
        Inc(CarryCount);
    if CarryCount >= MAX_CARRY then begin
      WriteLn('You can''t carry any more. Drop something first.');
      Exit;
    end;
    if not S.Items[ItemId].IsTakeable then begin
      if S.Items[ItemId].Name = 'PUMP' then
        WriteLn('The pump is fixed in place.')
      else if S.Items[ItemId].Name = 'HORSE' then
        WriteLn('It''s too big to carry.')
      else if S.Items[ItemId].Name = 'BOX' then
        WriteLn('It''s bolted down.')
      else if S.Items[ItemId].Name = 'ROCK' then
        WriteLn('It''s too heavy to carry.')
      else
        WriteLn('You can''t take that.');
      Exit;
    end;
    S.Items[ItemId].Location := INV_LOCATION;
    WriteLn('🎒 Taken: ', S.Items[ItemId].Description, '.');
    if not S.ItemScored[ItemId] then begin
      S.ItemScored[ItemId] := True;
      Inc(S.Score, SCORE_ITEM_PICKUP);
    end;
  end else
    WriteLn('Not here.');
end;

procedure HandleQuit(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  S.IsPlaying := False;
end;

procedure HandleClimb(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if S.CurrentRoom^.ID = 12 then MoveTo(S, S.RoomRegistry[STREAM_ROOM_ID])
  else WriteLn('There is nothing to climb here.');
end;

procedure HandlePut(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if Pos('SADDLE', UpperCase(Noun)) > 0 then SaddleHorse(S, 'HORSE', ConsumeTurn);
end;

procedure HandleDrop(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
var
  ItemId: Integer;
begin
  ItemId := FindItem(Noun, INV_LOCATION, S);
  if ItemId > 0 then begin
    S.Items[ItemId].Location := S.CurrentRoom^.ID;
    WriteLn('✋ Dropped: ', S.Items[ItemId].Description, '.');
  end else
    WriteLn('You aren''t carrying that.');
end;

procedure HandleBurn(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
var
  ItemId: Integer;
  Target: string;
begin
  Target := UpperCase(Trim(Noun));
  if Target = '' then begin
    WriteLn('Burn what?');
    Exit;
  end;
  if FindItem('MATCHES', INV_LOCATION, S) = 0 then begin
    WriteLn('You have nothing to burn it with.');
    Exit;
  end;
  ItemId := FindItem(Target, INV_LOCATION, S);
  if ItemId = 0 then
    ItemId := FindItem(Target, S.CurrentRoom^.ID, S);
  if ItemId = 0 then begin
    WriteLn('You don''t see that here.');
    Exit;
  end;
  if (S.Items[ItemId].Name <> 'BOOK') and (S.Items[ItemId].Name <> 'LEDGER') and
     (S.Items[ItemId].Name <> 'LEATHER') and (S.Items[ItemId].Name <> 'MAP') and
     (S.Items[ItemId].Name <> 'SADDLE') then begin
    WriteLn('It doesn''t burn.');
    Exit;
  end;
  S.Items[ItemId].Location := 0;
  WriteLn('You burn it to ash.');
end;

procedure HandleFire(var S: TGameState; const Noun: string; var ConsumeTurn: Boolean);
begin
  if FindItem('MATCHES', INV_LOCATION, S) = 0 then begin
    WriteLn('You have nothing to start a fire with.');
    Exit;
  end;
  if (S.CurrentRoom^.ID <> 2) and (S.CurrentRoom^.ID <> 3) and (S.CurrentRoom^.ID <> 5) then begin
    WriteLn('There is nothing here that will catch fire.');
    Exit;
  end;
  if S.RoomBurning[S.CurrentRoom^.ID] > 0 then begin
    WriteLn('A fire is already burning here.');
    Exit;
  end;
  S.RoomBurning[S.CurrentRoom^.ID] := 3;
  WriteLn('🔥 You start a fire. The room glows with heat.');
  if S.SnakeRoom = S.CurrentRoom^.ID then begin
    S.SnakeRoom := 0;
    WriteLn('🔥 The rattlesnake recoils from the flames and disappears.');
  end;
end;

function CheckHazards(var S: TGameState; const Verb: string): Boolean;
begin
  Result := True;
  if (S.SnakeRoom = S.CurrentRoom^.ID) and (Verb <> 'FREEZE') and (Verb <> 'WAIT') and
     (Verb <> 'N') and (Verb <> 'S') and (Verb <> 'E') and (Verb <> 'W') and
     (Verb <> 'NORTH') and (Verb <> 'SOUTH') and (Verb <> 'EAST') and (Verb <> 'WEST') and
     (Verb <> 'LOOK') and (Verb <> 'L') and (Verb <> 'EXAMINE') and (Verb <> 'X') and
     (Verb <> 'SEARCH') and (Verb <> 'INVENTORY') and (Verb <> 'I') and
     (Verb <> 'CHECK') and (Verb <> 'HELP') and (Verb <> '?') and (Verb <> 'H') and
     (Verb <> 'SCORE') and (Verb <> 'SAVE') and (Verb <> 'LOAD') and
     (Verb <> 'QUIT') and (Verb <> 'Q') then begin
    WriteLn;
    WrapWriteLn('🐍 As you reach out, the rattlesnake strikes! You feel a sharp pain in your hand.');
    WriteLn;
    WrapWriteLn('💀 The venom works quickly. GAME OVER.');
    S.IsPlaying := False;
    Result := False;
    Exit;
  end;

  if (S.OutlawRoom = S.CurrentRoom^.ID) and (Verb <> 'SHOOT') and (Verb <> 'KILL') and
     (Verb <> 'N') and (Verb <> 'S') and (Verb <> 'E') and (Verb <> 'W') and
     (Verb <> 'NORTH') and (Verb <> 'SOUTH') and (Verb <> 'EAST') and (Verb <> 'WEST') and
     (Verb <> 'LOOK') and (Verb <> 'L') and (Verb <> 'EXAMINE') and (Verb <> 'X') and
     (Verb <> 'SEARCH') and (Verb <> 'INVENTORY') and (Verb <> 'I') and
     (Verb <> 'CHECK') and (Verb <> 'HELP') and (Verb <> '?') and (Verb <> 'H') and
     (Verb <> 'SCORE') and (Verb <> 'SAVE') and (Verb <> 'LOAD') and
     (Verb <> 'QUIT') and (Verb <> 'Q') then begin
    WriteLn;
    WrapWriteLn('🤠 The outlaw doesn''t like you poking around. He draws his gun and fires.');
    WriteLn;
    WrapWriteLn('💥 Everything goes dark. GAME OVER.');
    S.IsPlaying := False;
    Result := False;
    Exit;
  end;
end;

procedure ProcessCommand(var S: TGameState; const Cmd: string);
var
  Verb, Noun: string;
  ConsumeTurn: Boolean;
  NounUpper: string;
  i: Integer;
  Handled: Boolean;
const
  Commands: array[1..33] of TCommandEntry = (
    (Verb: 'N'; Handler: nil),
    (Verb: 'NORTH'; Handler: nil),
    (Verb: 'S'; Handler: nil),
    (Verb: 'SOUTH'; Handler: nil),
    (Verb: 'E'; Handler: nil),
    (Verb: 'EAST'; Handler: nil),
    (Verb: 'W'; Handler: nil),
    (Verb: 'WEST'; Handler: nil),
    (Verb: 'LOOK'; Handler: @HandleLook),
    (Verb: 'L'; Handler: @HandleLook),
    (Verb: 'EXAMINE'; Handler: @ExamineItem),
    (Verb: 'X'; Handler: @ExamineItem),
    (Verb: 'SEARCH'; Handler: @HandleSearch),
    (Verb: 'HELP'; Handler: @ShowHelp),
    (Verb: '?'; Handler: @ShowHelp),
    (Verb: 'H'; Handler: @ShowHelp),
    (Verb: 'INVENTORY'; Handler: @HandleInventory),
    (Verb: 'I'; Handler: @HandleInventory),
    (Verb: 'INV'; Handler: @HandleInventory),
    (Verb: 'DRINK'; Handler: @Drink),
    (Verb: 'FILL'; Handler: @FillCanteen),
    (Verb: 'WATER'; Handler: @WaterHorse),
    (Verb: 'LIGHT'; Handler: @LightLamp),
    (Verb: 'FIX'; Handler: @FixSomething),
    (Verb: 'SADDLE'; Handler: @SaddleHorse),
    (Verb: 'PUT'; Handler: @HandlePut),
    (Verb: 'CLIMB'; Handler: @HandleClimb),
    (Verb: 'SAVE'; Handler: @HandleSave),
    (Verb: 'LOAD'; Handler: @HandleLoad),
    (Verb: 'DROP'; Handler: @HandleDrop),
    (Verb: 'D'; Handler: @HandleDrop),
    (Verb: 'BURN'; Handler: @HandleBurn),
    (Verb: 'FIRE'; Handler: @HandleFire)
  );
begin
  SplitCommand(Cmd, Verb, Noun);
  ConsumeTurn := True;

  if not CheckHazards(S, Verb) then Exit;

  if (Verb = 'MOUNT') or (Verb = 'RIDE') then begin
    HandleMount(S, Noun, ConsumeTurn);
  end else if Verb = 'DISMOUNT' then begin
    HandleDismount(S, Noun, ConsumeTurn);
  end else if Verb = 'OPEN' then begin
    NounUpper := UpperCase(Trim(Noun));
    HandleOpen(S, NounUpper, ConsumeTurn);
  end else if (Verb = 'SHOOT') or (Verb = 'KILL') then begin
    HandleShoot(S, Noun, ConsumeTurn);
  end else if (Verb = 'FREEZE') or (Verb = 'WAIT') then begin
    HandleFreeze(S, Noun, ConsumeTurn);
  end else if (Verb = 'CHECK') then begin
    NounUpper := UpperCase(Trim(Noun));
    if (NounUpper = 'INVENTORY') or (NounUpper = 'INV') or (NounUpper = 'I') then
      HandleInventory(S, Noun, ConsumeTurn)
  end else if (Verb = 'SCORE') then begin
    HandleScore(S, Noun, ConsumeTurn);
  end else if (Verb = 'TAKE') or (Verb = 'GET') then begin
    HandleTake(S, Noun, ConsumeTurn);
  end else if (Verb = 'QUIT') or (Verb = 'Q') then begin
    HandleQuit(S, Noun, ConsumeTurn);
  end else begin
    Handled := False;
    for i := Low(Commands) to High(Commands) do begin
      if Verb = Commands[i].Verb then begin
        if Commands[i].Handler = nil then begin
          if (Verb = 'N') or (Verb = 'NORTH') then begin
            if S.CurrentRoom^.North <> nil then PrintMovement('NORTH', S.IsRiding);
            MoveTo(S, S.CurrentRoom^.North);
          end else if (Verb = 'S') or (Verb = 'SOUTH') then begin
            if S.CurrentRoom^.South <> nil then PrintMovement('SOUTH', S.IsRiding);
            MoveTo(S, S.CurrentRoom^.South);
          end else if (Verb = 'E') or (Verb = 'EAST') then begin
            if S.CurrentRoom^.East <> nil then PrintMovement('EAST', S.IsRiding);
            MoveTo(S, S.CurrentRoom^.East);
          end else if (Verb = 'W') or (Verb = 'WEST') then begin
            if S.CurrentRoom^.West <> nil then PrintMovement('WEST', S.IsRiding);
            MoveTo(S, S.CurrentRoom^.West);
          end;
        end else
          Commands[i].Handler(S, Noun, ConsumeTurn);
        Handled := True;
        Break;
      end;
    end;
    if not Handled then begin
      WriteLn('🤷 I don''t know how to do that.');
    end;
  end;

  if S.IsPlaying and ConsumeTurn then UpdateWorld(S);
end;

end.
