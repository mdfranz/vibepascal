program Dustwood;

{$mode objfpc}{$H+}

uses
  SysUtils, IniFiles;

const
  MAX_ROOMS = 20;
  MAX_ITEMS = 20;
  INV_LOCATION = -1;

type
  PRoom = ^TRoom;
  TRoom = record
    ID: Integer;
    Name: string[50];
    Description: string[255];
    North, South, East, West: PRoom;
  end;

  TItem = record
    Name: string[30];
    Description: string[255];
    Details: string;
    Location: Integer; { -1 for Inv, 0 for hidden, >0 for Room ID }
    IsTakeable: Boolean;
  end;

var
  RoomRegistry: array[1..MAX_ROOMS] of PRoom;
  Items: array[1..MAX_ITEMS] of TItem;
  CurrentRoom: PRoom;
  IsPlaying, IsPumpFixed: Boolean;
  CommandStr: string;
  Verb, Noun: string;

{ --- Core Mechanics --- }

function FindItem(ItemName: string; Loc: Integer): Integer;
var i: Integer;
begin
  Result := 0; ItemName := UpperCase(ItemName);
  for i := 1 to MAX_ITEMS do
    if (Items[i].Location = Loc) and (Items[i].Name = ItemName) then begin Result := i; Exit; end;
end;

procedure Look;
var i: Integer; FoundItems: Boolean;
begin
  WriteLn; WriteLn('*** ', CurrentRoom^.Name, ' ***'); WriteLn(CurrentRoom^.Description);
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
  if NewRoom = nil then WriteLn('You cannot go that way.')
  else begin CurrentRoom := NewRoom; Look; end;
end;

procedure ShowHelp;
begin
  WriteLn;
  WriteLn('Available Commands:');
  WriteLn('  N, S, E, W      - Move North, South, East, West');
  WriteLn('  LOOK (L)        - Look around the current area');
  WriteLn('  EXAMINE (X)     - Look closely at an item (e.g., EXAMINE BOOK)');
  WriteLn('  TAKE (GET)      - Pick up an item (e.g., TAKE CANTEEN)');
  WriteLn('  DROP            - Leave an item behind');
  WriteLn('  INVENTORY (I)   - Check what you are carrying');
  WriteLn('  FIX [ITEM]      - Attempt to repair something');
  WriteLn('  HELP            - Show this list');
  WriteLn('  QUIT (Q)        - Exit the game');
  WriteLn;
end;

procedure ShowInventory;
var i: Integer; FoundItems: Boolean;
begin
  FoundItems := False; WriteLn('You are carrying:');
  for i := 1 to MAX_ITEMS do
    if Items[i].Location = INV_LOCATION then begin WriteLn('  - ', Items[i].Description); FoundItems := True; end;
  if not FoundItems then WriteLn('  Nothing.');
end;

procedure ExamineItem(TargetNoun: string);
var ItemID: Integer;
begin
  if (Copy(UpperCase(TargetNoun), 1, 3) = 'AT ') then TargetNoun := Trim(Copy(TargetNoun, 4, Length(TargetNoun)));
  ItemID := FindItem(TargetNoun, INV_LOCATION);
  if ItemID = 0 then ItemID := FindItem(TargetNoun, CurrentRoom^.ID);

  if ItemID > 0 then begin
    WriteLn(Items[ItemID].Details);
    { Puzzle: Reveal the note when examining the book }
    if (Items[ItemID].Name = 'BOOK') and (Items[5].Location = 0) then begin
      Items[5].Location := INV_LOCATION;
      WriteLn; WriteLn('Wait... as you open the book, a small folded note falls into your hand!');
    end;
  end
  else if TargetNoun = '' then Look
  else WriteLn('You don''t see that here.');
end;

procedure FixSomething(TargetNoun: string);
begin
  TargetNoun := UpperCase(TargetNoun);
  if (TargetNoun = 'PUMP') and (CurrentRoom^.ID = 3) then begin
    if IsPumpFixed then WriteLn('The pump is already working.')
    else if FindItem('LEATHER', INV_LOCATION) > 0 then begin
      IsPumpFixed := True;
      WriteLn('Using the tough leather scrap, you fashion a new gasket for the pump.');
      WriteLn('After a few stiff strokes, the pump begins to groan and spit out fresh, cold water!');
      Items[3].Description := 'a working water pump';
      Items[3].Details := 'The iron pump is now working smoothly, providing a steady stream of water.';
    end
    else WriteLn('You need something to use as a gasket to fix the pump.');
  end
  else WriteLn('You can''t fix that here.');
end;

{ --- Parser & Logic --- }

procedure InitGame;
var i: Integer; Ini: TIniFile; Section: string; N, S, E, W: Integer;
begin
  for i := 1 to MAX_ROOMS do RoomRegistry[i] := nil;
  if not FileExists('world.ini') then Halt(1);
  Ini := TIniFile.Create('world.ini');
  try
    for i := 1 to MAX_ROOMS do begin
      Section := 'Room' + IntToStr(i);
      if Ini.SectionExists(Section) then begin
        New(RoomRegistry[i]);
        FillChar(RoomRegistry[i]^, SizeOf(TRoom), 0);
        RoomRegistry[i]^.ID := i;
        RoomRegistry[i]^.Name := Ini.ReadString(Section, 'Name', '');
        RoomRegistry[i]^.Description := Ini.ReadString(Section, 'Description', '');
      end;
    end;
    for i := 1 to MAX_ROOMS do if RoomRegistry[i] <> nil then begin
      Section := 'Room' + IntToStr(i);
      N := Ini.ReadInteger(Section, 'North', 0); S := Ini.ReadInteger(Section, 'South', 0);
      E := Ini.ReadInteger(Section, 'East', 0); W := Ini.ReadInteger(Section, 'West', 0);
      if (N > 0) and (N <= MAX_ROOMS) then RoomRegistry[i]^.North := RoomRegistry[N];
      if (S > 0) and (S <= MAX_ROOMS) then RoomRegistry[i]^.South := RoomRegistry[S];
      if (E > 0) and (E <= MAX_ROOMS) then RoomRegistry[i]^.East := RoomRegistry[E];
      if (W > 0) and (W <= MAX_ROOMS) then RoomRegistry[i]^.West := RoomRegistry[W];
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
  CurrentRoom := RoomRegistry[1];
  if CurrentRoom = nil then begin
    WriteLn('Fatal Error: Starting room (Room1) not found in world.ini');
    Halt(1);
  end;
  IsPlaying := True; IsPumpFixed := False;
end;

procedure SplitCommand(Cmd: string; var V, N: string);
var SpacePos: Integer;
begin
  V := ''; N := ''; Cmd := Trim(Cmd); SpacePos := Pos(' ', Cmd);
  if SpacePos > 0 then begin
    V := UpperCase(Copy(Cmd, 1, SpacePos - 1));
    N := Trim(Copy(Cmd, SpacePos + 1, Length(Cmd)));
  end else V := UpperCase(Cmd);
end;

procedure ParseCommand(Cmd: string);
begin
  SplitCommand(Cmd, Verb, Noun);
  if (Verb = 'N') or (Verb = 'NORTH') then MoveTo(CurrentRoom^.North)
  else if (Verb = 'S') or (Verb = 'SOUTH') then MoveTo(CurrentRoom^.South)
  else if (Verb = 'E') or (Verb = 'EAST') then MoveTo(CurrentRoom^.East)
  else if (Verb = 'W') or (Verb = 'WEST') then MoveTo(CurrentRoom^.West)
  else if (Verb = 'LOOK') or (Verb = 'L') or (Verb = 'EXAMINE') or (Verb = 'X') then ExamineItem(Noun)
  else if (Verb = 'HELP') or (Verb = '?') then ShowHelp
  else if (Verb = 'INVENTORY') or (Verb = 'I') then ShowInventory
  else if (Verb = 'FIX') or (Verb = 'REPAIR') then FixSomething(Noun)
  else if (Verb = 'TAKE') or (Verb = 'GET') then
    begin
      if FindItem(Noun, CurrentRoom^.ID) > 0 then begin
        if Items[FindItem(Noun, CurrentRoom^.ID)].IsTakeable then begin
          Items[FindItem(Noun, CurrentRoom^.ID)].Location := INV_LOCATION;
          WriteLn('You took the ', Noun, '.');
        end else WriteLn('You can''t take that.');
      end else WriteLn('It isn''t here.');
    end
  else if (Verb = 'DROP') then
    begin
      if FindItem(Noun, INV_LOCATION) > 0 then begin
        Items[FindItem(Noun, INV_LOCATION)].Location := CurrentRoom^.ID;
        WriteLn('You dropped the ', Noun, '.');
      end else WriteLn('You aren''t carrying that.');
    end
  else if (Verb = 'QUIT') or (Verb = 'Q') then IsPlaying := False
  else if Verb <> '' then WriteLn('I don''t know how to do that.');
end;

begin
  InitGame;
  Look;
  while IsPlaying do begin Write('> '); ReadLn(CommandStr); ParseCommand(CommandStr); end;
end.
