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
  DARK_TURN = 30;
  TWILIGHT_TURN = 20;

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
  IsPlaying, IsPumpFixed, IsLampLit, HasWater: Boolean;
  CommandStr: string;
  Verb, Noun: string;
  Thirst, Turns, i: Integer;
  History: array[0..MAX_HISTORY] of string;
  HistoryCount: Integer = 0;

{ --- Custom Input --- }

function CustomReadLn(Prompt: string): string;
var Ch: Char; S: string; HistIdx: Integer;
begin
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

procedure Look;
var i: Integer; FoundItems: Boolean;
begin
  WriteLn;
  if IsDark then begin
    WriteLn('It is pitch black. You are likely to be eaten by a grue... or at least trip over a cactus.');
    Exit;
  end;

  if Turns >= DARK_TURN then WriteLn('[The moon hangs cold and silver in the black sky]')
  else if Turns >= TWILIGHT_TURN then WriteLn('[The sky is bruised purple, the sun sinking behind the peaks]');

  WriteLn('*** ', CurrentRoom^.Name, ' ***');
  WriteLn(CurrentRoom^.Description);
  
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
  else
  begin
    CurrentRoom := NewRoom;
    Look;
  end;
end;

procedure UpdateWorld;
begin
  Inc(Turns);
  Inc(Thirst);
  if Thirst > THIRST_LIMIT - 5 then
    WriteLn('*** Your throat is parched. You need water soon. ***');
  
  if Thirst >= THIRST_LIMIT then begin
    WriteLn('The desert has claimed you. Your strength fails, and you collapse into the dust.');
    WriteLn('GAME OVER.');
    IsPlaying := False;
  end;

  if Turns = TWILIGHT_TURN then WriteLn('The shadows are growing long. The sun is dipping low.');
  if Turns = DARK_TURN then WriteLn('The last of the light fades. Darkness swallows Dustwood.');
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
    WriteLn('The water is warm and tastes of tin, but it is the finest thing you''ve ever felt.');
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
    WriteLn('You strike a match and light the lamp. A warm yellow glow pushes back the shadows.');
  end;
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
  WriteLn('  LIGHT           - Light your lamp if you have matches');
  WriteLn('  FIX             - Repair something');
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
    WriteLn(Items[ItemID].Details);
    if (Items[ItemID].Name = 'BOOK') and (Items[5].Location = 0) then begin
      Items[5].Location := INV_LOCATION;
      WriteLn; WriteLn('Wait... a small folded note falls out of the book!');
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
      WriteLn('You fix the pump with the leather scrap. Water begins to flow!');
      Items[3].Description := 'a working water pump';
    end else WriteLn('You need a gasket to fix the pump.');
  end else WriteLn('Nothing to fix here.');
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
  CurrentRoom := RoomRegistry[1]; IsPlaying := True; IsPumpFixed := False; 
  IsLampLit := False; HasWater := False; Thirst := 0; Turns := 0;
end;

procedure ParseCommand(Cmd: string);
begin
  SplitCommand(Cmd, Verb, Noun);
  if (Verb = 'N') or (Verb = 'NORTH') then MoveTo(CurrentRoom^.North)
  else if (Verb = 'S') or (Verb = 'SOUTH') then MoveTo(CurrentRoom^.South)
  else if (Verb = 'E') or (Verb = 'EAST') then MoveTo(CurrentRoom^.East)
  else if (Verb = 'W') or (Verb = 'WEST') then MoveTo(CurrentRoom^.West)
  else if (Verb = 'LOOK') or (Verb = 'L') or (Verb = 'EXAMINE') or (Verb = 'X') then ExamineItem(Noun)
  else if (Verb = 'HELP') or (Verb = '?') or (Verb = 'H') then ShowHelp
  else if (Verb = 'INVENTORY') or (Verb = 'I') then begin
    WriteLn('You are carrying:');
    for i := 1 to MAX_ITEMS do if Items[i].Location = INV_LOCATION then WriteLn('  - ', Items[i].Description);
  end
  else if (Verb = 'DRINK') then Drink
  else if (Verb = 'FILL') then FillCanteen
  else if (Verb = 'LIGHT') then LightLamp
  else if (Verb = 'FIX') then FixSomething(Noun)
  else if (Verb = 'TAKE') or (Verb = 'GET') then begin
    i := FindItem(Noun, CurrentRoom^.ID);
    if i > 0 then begin Items[i].Location := INV_LOCATION; WriteLn('Taken.'); end else WriteLn('Not here.');
  end
  else if (Verb = 'QUIT') or (Verb = 'Q') then IsPlaying := False;
  
  if IsPlaying then UpdateWorld;
end;

begin
  InitGame; Look;
  while IsPlaying do ParseCommand(CustomReadLn('> '));
end.
