program Dustwood;

{$mode objfpc}{$H+}

uses
  SysUtils, IniFiles, Crt;

const
  MAX_ROOMS = 20;
  MAX_ITEMS = 20;
  INV_LOCATION = -1;
  MAX_HISTORY = 10;
  THIRST_LIMIT = 20;
  HORSE_THIRST_LIMIT = 15;
  DARK_TURN = 30;
  TWILIGHT_TURN = 20;
  SCORE_ROOM_VISIT = 5;
  SCORE_ITEM_PICKUP = 3;
  SCORE_NOTE_FOUND = 5;
  SCORE_PUMP_FIX = 20;
  SCORE_FIRST_FILL = 10;
  SCORE_LAMP_LIGHT = 5;
  SCORE_BOX_OPEN = 10;
  SCORE_OUTLAW_KILL = 15;
  STREAM_ROOM_ID = 13;
  DESERT_ENTRY_ROOM_ID = 8;

type
  PRoom = ^TRoom;
  TRoom = record
    ID: Integer;
    Name: string;
    Description: string;
    North, South, East, West: PRoom;
  end;

  TItem = record
    Name: string;
    Description: string;
    Details: string;
    Location: Integer;
    IsTakeable: Boolean;
  end;

var
  RoomRegistry: array[1..MAX_ROOMS] of PRoom;
  Items: array[1..MAX_ITEMS] of TItem;
  CurrentRoom: PRoom;
  IsPlaying, IsPumpFixed, IsLampLit, HasWater, IsHeadless, IsBoxOpen, IsHorseSaddled, IsRiding: Boolean;
  SnakeRoom: Integer; // ID of the room where a snake is currently coiled
  OutlawRoom: Integer; // ID of the room where the outlaw is waiting
  CommandStr: string;
  Verb, Noun: string;
  Thirst, Turns, HorseThirst, i: Integer;
  Score: Integer;
  RoomVisited: array[1..MAX_ROOMS] of Boolean;
  ItemScored: array[1..MAX_ITEMS] of Boolean;
  ScoredPumpFix, ScoredFirstFill, ScoredLampLight, ScoredBoxOpen, ScoredOutlawKill, ScoredNoteFound: Boolean;
  History: array[0..MAX_HISTORY] of string;
  HistoryCount: Integer = 0;

{ --- Custom Input --- }

procedure WrapWriteLn(S: string);
const MAX_WIDTH = 75;
var SpacePos: Integer;
begin
  while Length(S) > MAX_WIDTH do begin
    SpacePos := MAX_WIDTH + 1;
    while (SpacePos > 1) and (S[SpacePos] <> ' ') do Dec(SpacePos);
    if SpacePos = 1 then SpacePos := MAX_WIDTH;
    WriteLn(Copy(S, 1, SpacePos - 1));
    S := Trim(Copy(S, SpacePos, Length(S)));
  end;
  WriteLn(S);
end;

function CustomReadLn(Prompt: string): string;
var Ch: Char; S: string; HistIdx: Integer;
begin
  if IsHeadless then begin
    Write(Prompt);
    ReadLn(S);
    CustomReadLn := S;
    Exit;
  end;
  Write(Prompt); S := ''; HistIdx := HistoryCount;
  repeat
    Ch := ReadKey;
    if Ch = #0 then begin
      Ch := ReadKey;
      if Ch = #72 then begin
        if HistIdx > 0 then begin
          while Length(S) > 0 do begin Write(#8' '#8); Delete(S, Length(S), 1); end;
          Dec(HistIdx); S := History[HistIdx mod MAX_HISTORY]; Write(S);
        end;
      end else if Ch = #80 then begin
        if HistIdx < HistoryCount then begin
          while Length(S) > 0 do begin Write(#8' '#8); Delete(S, Length(S), 1); end;
          Inc(HistIdx); if HistIdx < HistoryCount then S := History[HistIdx mod MAX_HISTORY] else S := '';
          Write(S);
        end;
      end;
    end else if Ch = #8 then begin
      if Length(S) > 0 then begin Write(#8' '#8); Delete(S, Length(S), 1); end;
    end else if Ch = #4 then begin
      WriteLn('QUIT');
      Result := 'QUIT'; Exit;
    end else if Ch = #13 then begin
      WriteLn;
      if (S <> '') and ((HistoryCount = 0) or (S <> History[(HistoryCount-1) mod MAX_HISTORY])) then begin
        History[HistoryCount mod MAX_HISTORY] := S; Inc(HistoryCount);
      end;
      Result := S; Exit;
    end else if Ch >= #32 then begin S := S + Ch; Write(Ch); end;
  until False;
end;

{ --- Game Logic --- }

function FindItem(ItemName: string; Loc: Integer): Integer;
var i: Integer;
begin
  Result := 0; ItemName := UpperCase(ItemName);
  for i := 1 to MAX_ITEMS do
    if (Items[i].Location = Loc) and (Items[i].Name = ItemName) then begin Result := i; Exit; end;
end;

function IsDark: Boolean;
begin
  Result := (Turns >= DARK_TURN) and not IsLampLit;
end;

function IsDesertRoom(RoomID: Integer): Boolean;
begin
  Result := (RoomID >= 8) and (RoomID <= 13);
end;

procedure Look;
var i: Integer; FoundItems: Boolean;
begin
  WriteLn;
  if IsDark then begin
    WrapWriteLn('It is pitch black. You can''t see anything.');
    Exit;
  end;

  if Turns >= DARK_TURN then WriteLn('[The moon hangs in the black sky]')
  else if Turns >= TWILIGHT_TURN then WriteLn('[The sky is purple as the sun sets]');

  if IsRiding then Write('[RIDING] ');
  WriteLn('*** ', CurrentRoom^.Name, ' ***');
  WrapWriteLn(CurrentRoom^.Description);

  if SnakeRoom = CurrentRoom^.ID then begin
    WriteLn;
    WriteLn('!!! A RATTLESNAKE is coiled here, buzzing its tail angrily !!!');
    WriteLn('One wrong move could be your last.');
  end;

  if OutlawRoom = CurrentRoom^.ID then begin
    WriteLn;
    WriteLn('!!! A DIRTY OUTLAW is leaning against the wall, hand on his holster !!!');
    WriteLn('"You don''t belong here, stranger," he sneers.');
  end;
  
  FoundItems := False;
  for i := 1 to MAX_ITEMS do
    if Items[i].Location = CurrentRoom^.ID then begin
      if not FoundItems then begin WriteLn; WriteLn('You see the following here:'); FoundItems := True; end;
      WriteLn('  - ', Items[i].Description);
    end;
  WriteLn;
end;

procedure MoveTo(NewRoom: PRoom);
begin
  if NewRoom = nil then
    WriteLn('You cannot go that way.')
  else if IsRiding and ((NewRoom^.ID = 2) or (NewRoom^.ID = 4) or (NewRoom^.ID = 5) or (NewRoom^.ID = 7)) then
    WriteLn('You can''t bring a horse in there. Dismount first.')
  else if (CurrentRoom^.ID = 6) and (NewRoom^.ID = DESERT_ENTRY_ROOM_ID) and (not IsRiding) then
    WriteLn('The desert is too dangerous on foot. You must be riding a saddled horse.')
  else
  begin
    CurrentRoom := NewRoom;
    if IsRiding then begin
      for i := 1 to MAX_ITEMS do
        if Items[i].Name = 'HORSE' then begin
          Items[i].Location := CurrentRoom^.ID;
          Break;
        end;
    end;
    if (CurrentRoom^.ID <> 1) and (not RoomVisited[CurrentRoom^.ID]) then begin
      RoomVisited[CurrentRoom^.ID] := True;
      Inc(Score, SCORE_ROOM_VISIT);
    end;
    // 20% chance of a snake appearing in the new room (except Main Street)
    if (CurrentRoom^.ID <> 1) and (Random(100) < 20) then
      SnakeRoom := CurrentRoom^.ID
    else
      SnakeRoom := 0;

    // 15% chance of an outlaw appearing (except Main Street and Sheriff's office)
    if (CurrentRoom^.ID <> 1) and (CurrentRoom^.ID <> 7) and (Random(100) < 15) then
      OutlawRoom := CurrentRoom^.ID
    else
      OutlawRoom := 0;
    Look;
  end;
end;

procedure UpdateWorld;
begin
  Inc(Turns);
  Inc(Thirst);
  if IsHorseSaddled and IsDesertRoom(CurrentRoom^.ID) then Inc(HorseThirst);
  // Snakes eventually slither away
  if (SnakeRoom > 0) and (Random(100) < 30) then SnakeRoom := 0;
  if Thirst > THIRST_LIMIT - 5 then
    WriteLn('*** Your throat is parched. You need water soon. ***');
  if IsHorseSaddled and IsDesertRoom(CurrentRoom^.ID) and (HorseThirst > HORSE_THIRST_LIMIT - 5) then
    WriteLn('*** Your horse is showing signs of exhaustion. It needs water soon. ***');
  
  if Thirst >= THIRST_LIMIT then begin
    WrapWriteLn('You have collapsed from dehydration. GAME OVER.');
    IsPlaying := False;
  end;

  if IsDesertRoom(CurrentRoom^.ID) and (not IsRiding) then begin
    WrapWriteLn('The desert heat is overwhelming on foot. You collapse into the sand. GAME OVER.');
    IsPlaying := False;
  end;

  if IsHorseSaddled and IsDesertRoom(CurrentRoom^.ID) and (HorseThirst >= HORSE_THIRST_LIMIT) then begin
    WrapWriteLn('Your horse collapses from dehydration. You are stranded in the desert. GAME OVER.');
    IsPlaying := False;
  end;

  if Turns = TWILIGHT_TURN then WriteLn('The sun is getting low.');
  if Turns = DARK_TURN then WriteLn('It is now dark.');
end;

procedure Drink;
begin
  if FindItem('CANTEEN', INV_LOCATION) = 0 then
    WriteLn('You don''t have anything to drink from.')
  else if not HasWater then
    WriteLn('Your canteen is empty.')
  else begin
    Thirst := 0;
    HasWater := False;
    WrapWriteLn('The water is warm but refreshing.');
    WriteLn('Your thirst is quenched.');
  end;
end;

procedure FillCanteen;
begin
  if FindItem('CANTEEN', INV_LOCATION) = 0 then
    WriteLn('You have nothing to fill.')
  else if (CurrentRoom^.ID = 3) and IsPumpFixed then begin
    HasWater := True;
    WriteLn('You fill your canteen with fresh water from the pump.');
    if not ScoredFirstFill then begin
      ScoredFirstFill := True;
      Inc(Score, SCORE_FIRST_FILL);
    end;
  end else if (CurrentRoom^.ID = STREAM_ROOM_ID) then begin
    HasWater := True;
    WriteLn('You fill your canteen with cold stream water.');
  end else
    WriteLn('There is no water here.');
end;

procedure LightLamp;
begin
  if FindItem('LAMP', INV_LOCATION) = 0 then
    WriteLn('You don''t have a lamp.')
  else if FindItem('MATCHES', INV_LOCATION) = 0 then
    WriteLn('You have nothing to light it with.')
  else begin
    IsLampLit := True;
    WrapWriteLn('You light the lamp. A yellow glow illuminates the room.');
    if not ScoredLampLight then begin
      ScoredLampLight := True;
      Inc(Score, SCORE_LAMP_LIGHT);
    end;
  end;
end;

procedure SaveGame;
var Ini: TIniFile; i: Integer; Section: string; RoomsStr, ItemsStr: string;
begin
  Ini := TIniFile.Create('save.ini');
  try
    Ini.WriteInteger('State', 'CurrentRoom', CurrentRoom^.ID);
    Ini.WriteBool('State', 'IsPumpFixed', IsPumpFixed);
    Ini.WriteBool('State', 'IsLampLit', IsLampLit);
    Ini.WriteBool('State', 'HasWater', HasWater);
    Ini.WriteBool('State', 'IsHorseSaddled', IsHorseSaddled);
    Ini.WriteBool('State', 'IsRiding', IsRiding);
    Ini.WriteInteger('State', 'Thirst', Thirst);
    Ini.WriteInteger('State', 'HorseThirst', HorseThirst);
    Ini.WriteInteger('State', 'Turns', Turns);
    Ini.WriteInteger('State', 'Score', Score);
    for i := 1 to MAX_ITEMS do begin
      Section := 'Item' + IntToStr(i);
      Ini.WriteInteger(Section, 'Location', Items[i].Location);
      Ini.WriteString(Section, 'Description', Items[i].Description);
    end;
    RoomsStr := '';
    for i := 1 to MAX_ROOMS do
      if RoomVisited[i] then RoomsStr := RoomsStr + '1' else RoomsStr := RoomsStr + '0';
    ItemsStr := '';
    for i := 1 to MAX_ITEMS do
      if ItemScored[i] then ItemsStr := ItemsStr + '1' else ItemsStr := ItemsStr + '0';
    Ini.WriteString('ScoreFlags', 'RoomVisited', RoomsStr);
    Ini.WriteString('ScoreFlags', 'ItemScored', ItemsStr);
    Ini.WriteBool('ScoreFlags', 'ScoredPumpFix', ScoredPumpFix);
    Ini.WriteBool('ScoreFlags', 'ScoredFirstFill', ScoredFirstFill);
    Ini.WriteBool('ScoreFlags', 'ScoredLampLight', ScoredLampLight);
    Ini.WriteBool('ScoreFlags', 'ScoredBoxOpen', ScoredBoxOpen);
    Ini.WriteBool('ScoreFlags', 'ScoredOutlawKill', ScoredOutlawKill);
    Ini.WriteBool('ScoreFlags', 'ScoredNoteFound', ScoredNoteFound);
    WriteLn('Game saved.');
  finally Ini.Free; end;
end;

procedure LoadGame;
var Ini: TIniFile; i: Integer; Section: string; RoomID: Integer; RoomsStr, ItemsStr: string;
begin
  if not FileExists('save.ini') then begin
    WriteLn('No save file found.');
    Exit;
  end;
  Ini := TIniFile.Create('save.ini');
  try
    RoomID := Ini.ReadInteger('State', 'CurrentRoom', 1);
    CurrentRoom := RoomRegistry[RoomID];
    IsPumpFixed := Ini.ReadBool('State', 'IsPumpFixed', False);
    IsLampLit := Ini.ReadBool('State', 'IsLampLit', False);
    HasWater := Ini.ReadBool('State', 'HasWater', False);
    IsHorseSaddled := Ini.ReadBool('State', 'IsHorseSaddled', False);
    IsRiding := Ini.ReadBool('State', 'IsRiding', False);
    Thirst := Ini.ReadInteger('State', 'Thirst', 0);
    HorseThirst := Ini.ReadInteger('State', 'HorseThirst', 0);
    Turns := Ini.ReadInteger('State', 'Turns', 0);
    Score := Ini.ReadInteger('State', 'Score', 0);
    for i := 1 to MAX_ITEMS do begin
      Section := 'Item' + IntToStr(i);
      if Ini.SectionExists(Section) then begin
        Items[i].Location := Ini.ReadInteger(Section, 'Location', Items[i].Location);
        Items[i].Description := Ini.ReadString(Section, 'Description', Items[i].Description);
      end;
    end;
    RoomsStr := Ini.ReadString('ScoreFlags', 'RoomVisited', '');
    for i := 1 to MAX_ROOMS do begin
      if (Length(RoomsStr) >= i) and (RoomsStr[i] = '1') then RoomVisited[i] := True else RoomVisited[i] := False;
    end;
    ItemsStr := Ini.ReadString('ScoreFlags', 'ItemScored', '');
    for i := 1 to MAX_ITEMS do begin
      if (Length(ItemsStr) >= i) and (ItemsStr[i] = '1') then ItemScored[i] := True else ItemScored[i] := False;
    end;
    ScoredPumpFix := Ini.ReadBool('ScoreFlags', 'ScoredPumpFix', False);
    ScoredFirstFill := Ini.ReadBool('ScoreFlags', 'ScoredFirstFill', False);
    ScoredLampLight := Ini.ReadBool('ScoreFlags', 'ScoredLampLight', False);
    ScoredBoxOpen := Ini.ReadBool('ScoreFlags', 'ScoredBoxOpen', False);
    ScoredOutlawKill := Ini.ReadBool('ScoreFlags', 'ScoredOutlawKill', False);
    ScoredNoteFound := Ini.ReadBool('ScoreFlags', 'ScoredNoteFound', False);
    WriteLn('Game loaded.');
    Look;
  finally Ini.Free; end;
end;

{ --- Core Commands --- }

procedure ShowHelp;
begin
  WriteLn;
  WriteLn('Available Commands:');
  WriteLn('  N, S, E, W      - Move North, South, East, West');
  WriteLn('  LOOK (L)        - Look around');
  WriteLn('  EXAMINE (X)     - Look closely at an item');
  WriteLn('  TAKE (GET)      - Pick up an item');
  WriteLn('  DROP            - Leave an item');
  WriteLn('  INVENTORY (I)   - Check your gear');
  WriteLn('  DRINK           - Drink from your canteen');
  WriteLn('  FILL            - Fill canteen at a water source');
  WriteLn('  WATER           - Water your horse at a water source');
  WriteLn('  LIGHT           - Light your lamp if you have matches');
  WriteLn('  FIX             - Repair something');
  WriteLn('  SADDLE          - Put a saddle on the horse');
  WriteLn('  CLIMB           - Climb a steep obstacle');
  WriteLn('  SAVE / LOAD     - Save or load your progress');
  WriteLn('  SCORE           - Show current score');
  WriteLn('  HELP (H)        - Show this list');
  WriteLn('  QUIT (Q)        - Exit');
  WriteLn;
end;

procedure ExamineItem(TargetNoun: string);
var ItemID: Integer;
begin
  if (Copy(UpperCase(TargetNoun), 1, 3) = 'AT ') then TargetNoun := Trim(Copy(TargetNoun, 4, Length(TargetNoun)));
  ItemID := FindItem(TargetNoun, INV_LOCATION);
  if ItemID = 0 then ItemID := FindItem(TargetNoun, CurrentRoom^.ID);
  if ItemID > 0 then begin
    WrapWriteLn(Items[ItemID].Details);
    if (Items[ItemID].Name = 'ROCK') and (Items[11].Location = 0) then begin
      Items[11].Location := CurrentRoom^.ID;
      WriteLn;
      WriteLn('You lift the rock. A small brass key is hidden beneath it.');
    end;
    if (Items[ItemID].Name = 'BOOK') and (Items[5].Location = 0) then begin
      Items[5].Location := INV_LOCATION;
      WriteLn; WriteLn('A small folded note falls out of the book.');
      if not ScoredNoteFound then begin
        ScoredNoteFound := True;
        Inc(Score, SCORE_NOTE_FOUND);
      end;
    end;
  end
  else if TargetNoun = '' then Look
  else WriteLn('You don''t see that here.');
end;



procedure FixSomething(TargetNoun: string);
begin
  if (UpperCase(TargetNoun) = 'PUMP') and (CurrentRoom^.ID = 3) then begin
    if FindItem('LEATHER', INV_LOCATION) > 0 then begin
      IsPumpFixed := True;
      WriteLn('You fix the pump. Water starts to flow.');
      Items[3].Description := 'a working water pump';
      if not ScoredPumpFix then begin
        ScoredPumpFix := True;
        Inc(Score, SCORE_PUMP_FIX);
      end;
    end else WriteLn('You need a gasket.');
  end else WriteLn('Nothing to fix here.');
end;

procedure WaterHorse(TargetNoun: string);
begin
  TargetNoun := UpperCase(Trim(TargetNoun));
  if (TargetNoun <> '') and (TargetNoun <> 'HORSE') then begin
    WriteLn('Water what?');
    Exit;
  end;
  if not IsHorseSaddled then begin
    WriteLn('You don''t have a horse with you.');
    Exit;
  end;
  if CurrentRoom^.ID <> STREAM_ROOM_ID then begin
    WriteLn('There is no water here for your horse.');
    Exit;
  end;
  HorseThirst := 0;
  WriteLn('Your horse drinks deeply from the stream.');
end;

procedure SaddleHorse(TargetNoun: string);
var HorseID: Integer;
begin
  TargetNoun := UpperCase(Trim(TargetNoun));
  if (TargetNoun <> '') and (TargetNoun <> 'HORSE') and (TargetNoun <> 'ON HORSE') then begin
    WriteLn('Saddle what?');
    Exit;
  end;
  HorseID := FindItem('HORSE', CurrentRoom^.ID);
  if HorseID = 0 then begin
    WriteLn('There is no horse here.');
    Exit;
  end;
  if FindItem('SADDLE', INV_LOCATION) = 0 then begin
    WriteLn('You need a saddle.');
    Exit;
  end;
  if IsHorseSaddled then begin
    WriteLn('The horse is already saddled.');
    Exit;
  end;
  IsHorseSaddled := True;
  Items[HorseID].Description := 'a saddled horse';
  Items[HorseID].Details := 'A calm, saddle-ready horse. It looks steady and patient.';
  WriteLn('You secure the saddle onto the horse. It stands quietly.');
end;

{ --- Parser --- }

procedure SplitCommand(Cmd: string; var V, N: string);
var SpacePos: Integer;
begin
  V := ''; N := ''; Cmd := Trim(Cmd); SpacePos := Pos(' ', Cmd);
  if SpacePos > 0 then begin
    V := UpperCase(Copy(Cmd, 1, SpacePos - 1));
    N := Trim(Copy(Cmd, SpacePos + 1, Length(Cmd)));
  end else V := UpperCase(Cmd);
end;

procedure InitGame;
var i: Integer; Ini: TIniFile; Section: string; N, S, E, W: Integer;
begin
  for i := 1 to MAX_ROOMS do RoomRegistry[i] := nil;
  Ini := TIniFile.Create('world.ini');
  try
    for i := 1 to MAX_ROOMS do begin
      Section := 'Room' + IntToStr(i);
      if Ini.SectionExists(Section) then begin
        New(RoomRegistry[i]); RoomRegistry[i]^.ID := i;
        RoomRegistry[i]^.Name := Ini.ReadString(Section, 'Name', '');
        RoomRegistry[i]^.Description := Ini.ReadString(Section, 'Description', '');
        RoomRegistry[i]^.North := nil; RoomRegistry[i]^.South := nil;
        RoomRegistry[i]^.East := nil; RoomRegistry[i]^.West := nil;
      end;
    end;
    for i := 1 to MAX_ROOMS do if RoomRegistry[i] <> nil then begin
      Section := 'Room' + IntToStr(i);
      N := Ini.ReadInteger(Section, 'North', 0); S := Ini.ReadInteger(Section, 'South', 0);
      E := Ini.ReadInteger(Section, 'East', 0); W := Ini.ReadInteger(Section, 'West', 0);
      if (N > 0) then RoomRegistry[i]^.North := RoomRegistry[N];
      if (S > 0) then RoomRegistry[i]^.South := RoomRegistry[S];
      if (E > 0) then RoomRegistry[i]^.East := RoomRegistry[E];
      if (W > 0) then RoomRegistry[i]^.West := RoomRegistry[W];
    end;
    for i := 1 to MAX_ITEMS do begin
      Section := 'Item' + IntToStr(i);
      if Ini.SectionExists(Section) then begin
        Items[i].Name := UpperCase(Ini.ReadString(Section, 'Name', ''));
        Items[i].Description := Ini.ReadString(Section, 'Description', '');
        Items[i].Details := Ini.ReadString(Section, 'Details', '');
        Items[i].Location := Ini.ReadInteger(Section, 'Location', 0);
        Items[i].IsTakeable := Ini.ReadInteger(Section, 'IsTakeable', 0) = 1;
      end;
    end;
  finally Ini.Free; end;
  Randomize;
  SnakeRoom := 0;
  OutlawRoom := 0;
  IsBoxOpen := False;
  IsHorseSaddled := False;
  Score := 0;
  HorseThirst := 0;
  for i := 1 to MAX_ROOMS do RoomVisited[i] := False;
  for i := 1 to MAX_ITEMS do ItemScored[i] := False;
  ScoredPumpFix := False;
  ScoredFirstFill := False;
  ScoredLampLight := False;
  ScoredBoxOpen := False;
  ScoredOutlawKill := False;
  ScoredNoteFound := False;
  CurrentRoom := RoomRegistry[1]; IsPlaying := True; IsPumpFixed := False; 
  IsLampLit := False; HasWater := False; Thirst := 0; Turns := 0;
end;

procedure ParseCommand(Cmd: string);
var ConsumeTurn: Boolean;
begin
  SplitCommand(Cmd, Verb, Noun);
  ConsumeTurn := True;
  
  // If a snake is here, any action other than moving or FREEZE is dangerous
  if (SnakeRoom = CurrentRoom^.ID) and (Verb <> 'FREEZE') and (Verb <> 'WAIT') and 
     (Verb <> 'N') and (Verb <> 'S') and (Verb <> 'E') and (Verb <> 'W') and
     (Verb <> 'NORTH') and (Verb <> 'SOUTH') and (Verb <> 'EAST') and (Verb <> 'WEST') and
     (Verb <> 'LOOK') and (Verb <> 'L') and (Verb <> 'EXAMINE') and (Verb <> 'X') and
     (Verb <> 'SEARCH') and (Verb <> 'INVENTORY') and (Verb <> 'I') and
     (Verb <> 'CHECK') and (Verb <> 'HELP') and (Verb <> '?') and (Verb <> 'H') and
     (Verb <> 'SCORE') and (Verb <> 'SAVE') and (Verb <> 'LOAD') and
     (Verb <> 'QUIT') and (Verb <> 'Q') then
  begin
    WrapWriteLn('As you reach out, the rattlesnake strikes! You feel a sharp pain in your hand.');
    WrapWriteLn('The venom works quickly. GAME OVER.');
    IsPlaying := False;
    Exit;
  end;

  if (OutlawRoom = CurrentRoom^.ID) and (Verb <> 'SHOOT') and (Verb <> 'KILL') and
     (Verb <> 'N') and (Verb <> 'S') and (Verb <> 'E') and (Verb <> 'W') and
     (Verb <> 'NORTH') and (Verb <> 'SOUTH') and (Verb <> 'EAST') and (Verb <> 'WEST') and
     (Verb <> 'LOOK') and (Verb <> 'L') and (Verb <> 'EXAMINE') and (Verb <> 'X') and
     (Verb <> 'SEARCH') and (Verb <> 'INVENTORY') and (Verb <> 'I') and
     (Verb <> 'CHECK') and (Verb <> 'HELP') and (Verb <> '?') and (Verb <> 'H') and
     (Verb <> 'SCORE') and (Verb <> 'SAVE') and (Verb <> 'LOAD') and
     (Verb <> 'QUIT') and (Verb <> 'Q') then
  begin
    WrapWriteLn('The outlaw doesn''t like you poking around. He draws his gun and fires.');
    WrapWriteLn('Everything goes dark. GAME OVER.');
    IsPlaying := False;
    Exit;
  end;

  if (Verb = 'MOUNT') or (Verb = 'RIDE') then begin
    if IsRiding then WriteLn('You are already riding.')
    else if FindItem('HORSE', CurrentRoom^.ID) > 0 then begin
      if IsHorseSaddled then begin
        IsRiding := True;
        WriteLn('You swing yourself into the saddle. You are now riding.');
      end else WriteLn('The horse needs a saddle before you can ride her.');
    end else WriteLn('There is no horse here.');
  end
  else if (Verb = 'DISMOUNT') then begin
    if not IsRiding then WriteLn('You aren''t riding anything.')
    else begin
      IsRiding := False;
      WriteLn('You dismount and stand beside your horse.');
    end;
  end
  else if (Verb = 'OPEN') and (Noun = 'BOX') and (CurrentRoom^.ID = 7) then begin
    if IsBoxOpen then WriteLn('It is already open.')
    else begin
      if FindItem('KEY', INV_LOCATION) = 0 then
        WriteLn('The box is locked. You need a key.')
      else begin
        IsBoxOpen := True;
        Items[8].Location := 7; // Put revolver in Sheriff's office
        WriteLn('You unlock the box. Inside lies a heavy revolver.');
        if not ScoredBoxOpen then begin
          ScoredBoxOpen := True;
          Inc(Score, SCORE_BOX_OPEN);
        end;
      end;
    end;
  end
  else if (Verb = 'SHOOT') or (Verb = 'KILL') then begin
    if FindItem('REVOLVER', INV_LOCATION) = 0 then
      WriteLn('You have nothing to shoot with.')
    else if OutlawRoom = CurrentRoom^.ID then begin
      OutlawRoom := 0;
      WrapWriteLn('You draw your revolver and fire first. The outlaw falls to the ground.');
      WriteLn('The threat is gone.');
      if not ScoredOutlawKill then begin
        ScoredOutlawKill := True;
        Inc(Score, SCORE_OUTLAW_KILL);
      end;
    end else
      WriteLn('Nothing here to shoot.');
  end
  else if (Verb = 'FREEZE') or (Verb = 'WAIT') then begin
    WriteLn('You stay perfectly still. The snake watches you...');
    if Random(100) < 50 then begin
      SnakeRoom := 0;
      WriteLn('The snake loses interest and slithers into the shadows.');
    end;
  end
  else if (Verb = 'N') or (Verb = 'NORTH') then MoveTo(CurrentRoom^.North)
  else if (Verb = 'S') or (Verb = 'SOUTH') then MoveTo(CurrentRoom^.South)
  else if (Verb = 'E') or (Verb = 'EAST') then MoveTo(CurrentRoom^.East)
  else if (Verb = 'W') or (Verb = 'WEST') then MoveTo(CurrentRoom^.West)
  else if (Verb = 'LOOK') or (Verb = 'L') then begin
    ExamineItem(Noun);
    ConsumeTurn := False;
  end
  else if (Verb = 'EXAMINE') or (Verb = 'X') then begin
    ExamineItem(Noun);
    ConsumeTurn := False;
  end
  else if (Verb = 'SEARCH') then begin
    Look;
    ConsumeTurn := False;
  end
  else if (Verb = 'HELP') or (Verb = '?') or (Verb = 'H') then begin
    ShowHelp;
    ConsumeTurn := False;
  end
  else if (Verb = 'INVENTORY') or (Verb = 'I') then begin
    WriteLn('You are carrying:');
    for i := 1 to MAX_ITEMS do if Items[i].Location = INV_LOCATION then WriteLn('  - ', Items[i].Description);
    ConsumeTurn := False;
  end
  else if (Verb = 'CHECK') and ((UpperCase(Noun) = 'INVENTORY') or (UpperCase(Noun) = 'INV') or (UpperCase(Noun) = 'I')) then begin
    WriteLn('You are carrying:');
    for i := 1 to MAX_ITEMS do if Items[i].Location = INV_LOCATION then WriteLn('  - ', Items[i].Description);
    ConsumeTurn := False;
  end
  else if (Verb = 'DRINK') then Drink
  else if (Verb = 'FILL') then FillCanteen
  else if (Verb = 'WATER') then WaterHorse(Noun)
  else if (Verb = 'LIGHT') then LightLamp
  else if (Verb = 'FIX') then FixSomething(Noun)
  else if (Verb = 'SADDLE') then SaddleHorse(Noun)
  else if (Verb = 'PUT') and (Pos('SADDLE', UpperCase(Noun)) > 0) then SaddleHorse('HORSE')
  else if (Verb = 'CLIMB') then begin
    if (CurrentRoom^.ID = 12) then MoveTo(RoomRegistry[STREAM_ROOM_ID])
    else WriteLn('There is nothing to climb here.');
  end
  else if (Verb = 'SAVE') then begin
    SaveGame;
    ConsumeTurn := False;
  end
  else if (Verb = 'LOAD') then begin
    LoadGame;
    ConsumeTurn := False;
  end
  else if (Verb = 'SCORE') then begin
    WriteLn('Score: ', Score);
    ConsumeTurn := False;
  end
  else if (Verb = 'TAKE') or (Verb = 'GET') then begin
    i := FindItem(Noun, CurrentRoom^.ID);
    if i > 0 then begin
      Items[i].Location := INV_LOCATION;
      WriteLn('Taken: ', Items[i].Description, '.');
      if not ItemScored[i] then begin
        ItemScored[i] := True;
        Inc(Score, SCORE_ITEM_PICKUP);
      end;
    end else WriteLn('Not here.');
  end
  else if (Verb = 'QUIT') or (Verb = 'Q') then IsPlaying := False;
  
  if IsPlaying and ConsumeTurn then UpdateWorld;
end;

begin
  IsHeadless := (ParamCount > 0) and (ParamStr(1) = '--headless');
  InitGame; Look;
  while IsPlaying do ParseCommand(CustomReadLn('> '));
  WriteLn('Final score: ', Score);
end.
