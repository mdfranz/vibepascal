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
    Location: Integer; { -1 for Inv, or Room ID }
    IsTakeable: Boolean;
  end;

var
  RoomRegistry: array[1..MAX_ROOMS] of PRoom;
  Items: array[1..MAX_ITEMS] of TItem;
  CurrentRoom: PRoom;
  IsPlaying: Boolean;
  CommandStr: string;
  Verb, Noun: string;

{ --- Core Mechanics --- }

function FindItemInCurrentRoom(ItemName: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to MAX_ITEMS do
  begin
    if (Items[i].Location = CurrentRoom^.ID) and (Items[i].Name = ItemName) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function FindItemInInventory(ItemName: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to MAX_ITEMS do
  begin
    if (Items[i].Location = INV_LOCATION) and (Items[i].Name = ItemName) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure Look;
var
  i: Integer;
  FoundItems: Boolean;
begin
  WriteLn;
  WriteLn('*** ', CurrentRoom^.Name, ' ***');
  WriteLn(CurrentRoom^.Description);
  
  FoundItems := False;
  for i := 1 to MAX_ITEMS do
  begin
    if Items[i].Location = CurrentRoom^.ID then
    begin
      if not FoundItems then
      begin
        WriteLn;
        WriteLn('You see the following here:');
        FoundItems := True;
      end;
      WriteLn('  - ', Items[i].Name);
    end;
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

{ --- Initialization --- }

procedure InitGame;
var
  i: Integer;
  Ini: TIniFile;
  Section: string;
  N, S, E, W: Integer;
begin
  { Initialize Registry }
  for i := 1 to MAX_ROOMS do RoomRegistry[i] := nil;

  if not FileExists('world.ini') then
  begin
    WriteLn('Error: world.ini not found!');
    Halt(1);
  end;

  Ini := TIniFile.Create('world.ini');
  try
    { Pass 1: Allocate Rooms }
    for i := 1 to MAX_ROOMS do
    begin
      Section := 'Room' + IntToStr(i);
      if Ini.SectionExists(Section) then
      begin
        New(RoomRegistry[i]);
        RoomRegistry[i]^.ID := i;
        RoomRegistry[i]^.Name := Ini.ReadString(Section, 'Name', 'Empty Room');
        RoomRegistry[i]^.Description := Ini.ReadString(Section, 'Description', '');
        RoomRegistry[i]^.North := nil; RoomRegistry[i]^.South := nil;
        RoomRegistry[i]^.East := nil; RoomRegistry[i]^.West := nil;
      end;
    end;

    { Pass 2: Link Exits using pointers }
    for i := 1 to MAX_ROOMS do
    begin
      if RoomRegistry[i] <> nil then
      begin
        Section := 'Room' + IntToStr(i);
        N := Ini.ReadInteger(Section, 'North', 0);
        S := Ini.ReadInteger(Section, 'South', 0);
        E := Ini.ReadInteger(Section, 'East', 0);
        W := Ini.ReadInteger(Section, 'West', 0);

        if (N > 0) and (N <= MAX_ROOMS) then RoomRegistry[i]^.North := RoomRegistry[N];
        if (S > 0) and (S <= MAX_ROOMS) then RoomRegistry[i]^.South := RoomRegistry[S];
        if (E > 0) and (E <= MAX_ROOMS) then RoomRegistry[i]^.East := RoomRegistry[E];
        if (W > 0) and (W <= MAX_ROOMS) then RoomRegistry[i]^.West := RoomRegistry[W];
      end;
    end;

    { Load Items }
    for i := 1 to MAX_ITEMS do
    begin
      Section := 'Item' + IntToStr(i);
      if Ini.SectionExists(Section) then
      begin
        Items[i].Name := Ini.ReadString(Section, 'Name', '');
        Items[i].Description := Ini.ReadString(Section, 'Description', '');
        Items[i].Location := Ini.ReadInteger(Section, 'Location', 0);
        Items[i].IsTakeable := Ini.ReadInteger(Section, 'IsTakeable', 0) = 1;
      end;
    end;
  finally
    Ini.Free;
  end;

  CurrentRoom := RoomRegistry[1];
  IsPlaying := True;

  WriteLn('--------------------------------------------------');
  WriteLn('              ECHOES OF DUSTWOOD                  ');
  WriteLn('--------------------------------------------------');
  WriteLn('The year is 1884. You are Elias Thorne.');
  WriteLn('--------------------------------------------------');
  WriteLn;
end;

{ --- Parser & Commands --- }

procedure ShowInventory;
var
  i: Integer;
  FoundItems: Boolean;
begin
  FoundItems := False;
  WriteLn('You are carrying:');
  for i := 1 to MAX_ITEMS do
  begin
    if Items[i].Location = INV_LOCATION then
    begin
      WriteLn('  - ', Items[i].Name);
      FoundItems := True;
    end;
  end;
  if not FoundItems then WriteLn('  Nothing.');
end;

procedure TakeItem(TargetNoun: string);
var
  ItemID: Integer;
begin
  ItemID := FindItemInCurrentRoom(TargetNoun);
  if ItemID > 0 then
  begin
    if Items[ItemID].IsTakeable then
    begin
      Items[ItemID].Location := INV_LOCATION;
      WriteLn('You picked up the ', Items[ItemID].Name, '.');
    end
    else WriteLn('You cannot take that.');
  end
  else WriteLn('You don''t see that here.');
end;

procedure DropItem(TargetNoun: string);
var
  ItemID: Integer;
begin
  ItemID := FindItemInInventory(TargetNoun);
  if ItemID > 0 then
  begin
    Items[ItemID].Location := CurrentRoom^.ID;
    WriteLn('You dropped the ', Items[ItemID].Name, '.');
  end
  else WriteLn('You aren''t carrying that.');
end;

procedure ExamineItem(TargetNoun: string);
var
  ItemID: Integer;
begin
  ItemID := FindItemInInventory(TargetNoun);
  if ItemID = 0 then ItemID := FindItemInCurrentRoom(TargetNoun);

  if ItemID > 0 then WriteLn(Items[ItemID].Description)
  else WriteLn('You don''t see that here.');
end;

procedure SplitCommand(Cmd: string; var V, N: string);
var SpacePos: Integer;
begin
  V := ''; N := '';
  Cmd := UpperCase(Trim(Cmd));
  SpacePos := Pos(' ', Cmd);
  if SpacePos > 0 then
  begin
    V := Copy(Cmd, 1, SpacePos - 1);
    N := Trim(Copy(Cmd, SpacePos + 1, Length(Cmd)));
  end
  else V := Cmd;
end;

procedure ParseCommand(Cmd: string);
begin
  SplitCommand(Cmd, Verb, Noun);
  if (Verb = 'N') or (Verb = 'NORTH') then MoveTo(CurrentRoom^.North)
  else if (Verb = 'S') or (Verb = 'SOUTH') then MoveTo(CurrentRoom^.South)
  else if (Verb = 'E') or (Verb = 'EAST') then MoveTo(CurrentRoom^.East)
  else if (Verb = 'W') or (Verb = 'WEST') then MoveTo(CurrentRoom^.West)
  else if (Verb = 'LOOK') or (Verb = 'L') then Look
  else if (Verb = 'QUIT') or (Verb = 'Q') then IsPlaying := False
  else if (Verb = 'INVENTORY') or (Verb = 'I') then ShowInventory
  else if (Verb = 'TAKE') or (Verb = 'GET') then TakeItem(Noun)
  else if (Verb = 'DROP') then DropItem(Noun)
  else if (Verb = 'EXAMINE') or (Verb = 'X') then ExamineItem(Noun)
  else if Verb <> '' then WriteLn('I don''t know how to do that.');
end;

begin
  InitGame;
  Look;
  while IsPlaying do
  begin
    Write('> ');
    ReadLn(CommandStr);
    ParseCommand(CommandStr);
  end;
end.
