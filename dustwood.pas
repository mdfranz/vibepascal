program Dustwood;

{$mode objfpc}{$H+}

uses
  SysUtils, IniFiles;

const
  MAX_ROOMS = 10;
  MAX_ITEMS = 15;
  INV_LOCATION = -1; { Special location ID for items in the player's inventory }

type
  TLocation = record
    Name: string[50];
    Description: string[255];
    North, South, East, West: Integer;
  end;

  TItem = record
    ID: Integer;
    Name: string[30];
    Description: string[255];
    Location: Integer;     { Room ID, INV_LOCATION, or 0 if inactive/hidden }
    IsTakeable: Boolean;
  end;

var
  Rooms: array[1..MAX_ROOMS] of TLocation;
  Items: array[1..MAX_ITEMS] of TItem;
  CurrentRoom: Integer;
  IsPlaying: Boolean;
  CommandStr: string;
  Verb, Noun: string;

{ --- Initialization --- }

procedure InitGame;
var
  i: Integer;
  Ini: TIniFile;
  Section: string;
begin
  { Initialize arrays first }
  for i := 1 to MAX_ROOMS do
  begin
    Rooms[i].Name := '';
    Rooms[i].Description := '';
    Rooms[i].North := 0; Rooms[i].South := 0; Rooms[i].East := 0; Rooms[i].West := 0;
  end;
  for i := 1 to MAX_ITEMS do
  begin
    Items[i].ID := i;
    Items[i].Name := '';
    Items[i].Description := '';
    Items[i].Location := 0;
    Items[i].IsTakeable := False;
  end;

  { Load data from INI }
  if FileExists('world.ini') then
  begin
    Ini := TIniFile.Create('world.ini');
    try
      for i := 1 to MAX_ROOMS do
      begin
        Section := 'Room' + IntToStr(i);
        if Ini.SectionExists(Section) then
        begin
          Rooms[i].Name := Ini.ReadString(Section, 'Name', '');
          Rooms[i].Description := Ini.ReadString(Section, 'Description', '');
          Rooms[i].North := Ini.ReadInteger(Section, 'North', 0);
          Rooms[i].South := Ini.ReadInteger(Section, 'South', 0);
          Rooms[i].East := Ini.ReadInteger(Section, 'East', 0);
          Rooms[i].West := Ini.ReadInteger(Section, 'West', 0);
        end;
      end;
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
  end
  else
  begin
    WriteLn('Error: world.ini not found!');
    Halt(1);
  end;

  CurrentRoom := 1;
  IsPlaying := True;

  WriteLn('--------------------------------------------------');
  WriteLn('              ECHOES OF DUSTWOOD                  ');
  WriteLn('--------------------------------------------------');
  WriteLn('The year is 1884. You are Elias Thorne. You rode');
  WriteLn('into Dustwood looking for your brother Silas.');
  WriteLn('Your horse is dead. The town is abandoned.');
  WriteLn('Find him, find the truth, and survive.');
  WriteLn('--------------------------------------------------');
  WriteLn('Commands: N, S, E, W, LOOK, TAKE [ITEM], DROP [ITEM],');
  WriteLn('          EXAMINE [ITEM], INVENTORY (I), QUIT (Q)');
  WriteLn;
end;

{ --- Core Mechanics --- }

function FindItem(ItemName: string; CheckLocation: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to MAX_ITEMS do
  begin
    if (Items[i].Location = CheckLocation) and (Items[i].Name = ItemName) then
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
  WriteLn('*** ', Rooms[CurrentRoom].Name, ' ***');
  WriteLn(Rooms[CurrentRoom].Description);
  
  FoundItems := False;
  for i := 1 to MAX_ITEMS do
  begin
    if Items[i].Location = CurrentRoom then
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

procedure MoveTo(NewRoom: Integer);
begin
  if NewRoom = 0 then
    WriteLn('You cannot go that way.')
  else
  begin
    CurrentRoom := NewRoom;
    Look;
  end;
end;

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
  
  if not FoundItems then
    WriteLn('  Nothing.');
end;

procedure TakeItem(TargetNoun: string);
var
  ItemID: Integer;
begin
  ItemID := FindItem(TargetNoun, CurrentRoom);
  if ItemID > 0 then
  begin
    if Items[ItemID].IsTakeable then
    begin
      Items[ItemID].Location := INV_LOCATION;
      WriteLn('You picked up the ', Items[ItemID].Name, '.');
    end
    else
      WriteLn('You cannot take the ', Items[ItemID].Name, '.');
  end
  else
    WriteLn('You don''t see a ', TargetNoun, ' here.');
end;

procedure DropItem(TargetNoun: string);
var
  ItemID: Integer;
begin
  ItemID := FindItem(TargetNoun, INV_LOCATION);
  if ItemID > 0 then
  begin
    Items[ItemID].Location := CurrentRoom;
    WriteLn('You dropped the ', Items[ItemID].Name, '.');
  end
  else
    WriteLn('You are not carrying a ', TargetNoun, '.');
end;

procedure ExamineItem(TargetNoun: string);
var
  ItemID: Integer;
begin
  { Check inventory first, then room }
  ItemID := FindItem(TargetNoun, INV_LOCATION);
  if ItemID = 0 then
    ItemID := FindItem(TargetNoun, CurrentRoom);

  if ItemID > 0 then
    WriteLn(Items[ItemID].Description)
  else
    WriteLn('You don''t see a ', TargetNoun, ' to examine.');
end;

{ --- Parser --- }

procedure SplitCommand(Cmd: string; var V, N: string);
var
  SpacePos: Integer;
begin
  V := '';
  N := '';
  Cmd := UpperCase(Trim(Cmd));
  SpacePos := Pos(' ', Cmd);
  
  if SpacePos > 0 then
  begin
    V := Copy(Cmd, 1, SpacePos - 1);
    N := Trim(Copy(Cmd, SpacePos + 1, Length(Cmd)));
  end
  else
    V := Cmd;
end;

procedure ParseCommand(Cmd: string);
begin
  SplitCommand(Cmd, Verb, Noun);

  if (Verb = 'N') or (Verb = 'NORTH') then MoveTo(Rooms[CurrentRoom].North)
  else if (Verb = 'S') or (Verb = 'SOUTH') then MoveTo(Rooms[CurrentRoom].South)
  else if (Verb = 'E') or (Verb = 'EAST') then MoveTo(Rooms[CurrentRoom].East)
  else if (Verb = 'W') or (Verb = 'WEST') then MoveTo(Rooms[CurrentRoom].West)
  else if (Verb = 'LOOK') or (Verb = 'L') then Look
  else if (Verb = 'QUIT') or (Verb = 'Q') then IsPlaying := False
  else if (Verb = 'INVENTORY') or (Verb = 'I') then ShowInventory
  else if (Verb = 'TAKE') or (Verb = 'GET') then TakeItem(Noun)
  else if (Verb = 'DROP') then DropItem(Noun)
  else if (Verb = 'EXAMINE') or (Verb = 'X') then ExamineItem(Noun)
  else if Verb <> '' then
    WriteLn('I don''t know how to do that.');
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

  WriteLn('You wander off into the dust... Game Over.');
end.
