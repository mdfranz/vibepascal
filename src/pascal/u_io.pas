unit u_io;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Crt, u_types, u_state;

procedure WrapWriteLn(S: string);
function CustomReadLn(var S: TGameState; const Prompt: string): string;
procedure Look(var S: TGameState);

implementation

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

procedure WrapWriteLn(S: string);
const
  MAX_WIDTH = 79;
var
  SpacePos: Integer;
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

function IsDark(const S: TGameState): Boolean;
begin
  Result := (S.Turns >= DARK_TURN) and not S.IsLampLit and (S.TempLightTurns = 0);
end;

function CustomReadLn(var S: TGameState; const Prompt: string): string;
var
  Ch: Char;
  Line: string;
  HistIdx: Integer;
begin
  if S.IsHeadless then begin
    Write(Prompt);
    ReadLn(Line);
    CustomReadLn := Line;
    Exit;
  end;
  Write(Prompt);
  Line := '';
  HistIdx := S.HistoryCount;
  repeat
    Ch := ReadKey;
    if Ch = #0 then begin
      Ch := ReadKey;
      if Ch = #72 then begin
        if HistIdx > 0 then begin
          while Length(Line) > 0 do begin
            Write(#8' '#8);
            Delete(Line, Length(Line), 1);
          end;
          Dec(HistIdx);
          Line := S.History[HistIdx mod MAX_HISTORY];
          Write(Line);
        end;
      end else if Ch = #80 then begin
        if HistIdx < S.HistoryCount then begin
          while Length(Line) > 0 do begin
            Write(#8' '#8);
            Delete(Line, Length(Line), 1);
          end;
          Inc(HistIdx);
          if HistIdx < S.HistoryCount then Line := S.History[HistIdx mod MAX_HISTORY] else Line := '';
          Write(Line);
        end;
      end;
    end else if Ch = #8 then begin
      if Length(Line) > 0 then begin
        Write(#8' '#8);
        Delete(Line, Length(Line), 1);
      end;
    end else if Ch = #4 then begin
      WriteLn('QUIT');
      Result := 'QUIT';
      Exit;
    end else if Ch = #13 then begin
      WriteLn;
      if (Line <> '') and ((S.HistoryCount = 0) or (Line <> S.History[(S.HistoryCount - 1) mod MAX_HISTORY])) then begin
        S.History[S.HistoryCount mod MAX_HISTORY] := Line;
        Inc(S.HistoryCount);
      end;
      Result := Line;
      Exit;
    end else if Ch >= #32 then begin
      Line := Line + Ch;
      Write(Ch);
    end;
  until False;
end;

procedure Look(var S: TGameState);
var
  i: Integer;
  FoundItems: Boolean;
  Exits: string;
begin
  WriteLn;
  if IsDark(S) then begin
    WrapWriteLn('🌑 It is pitch black. You can''t see anything.');
    Exit;
  end;

  if S.RoomBurning[S.CurrentRoom^.ID] > 0 then begin
    WriteLn('🔥 The room is lit by a growing fire.');
  end;

  if S.Turns >= DARK_TURN then WriteLn('🌕 [The moon hangs in the black sky]')
  else if S.Turns >= TWILIGHT_TURN then WriteLn('🌇 [The sky is purple as the sun sets]');

  if S.IsRiding then Write('🏇 ');
  Write('📍 === ', S.CurrentRoom^.Name, ' === ');
  case S.CurrentRoom^.ID of
    1: Write('🏘️');
    2: Write('📟');
    3: Write('🐴');
    4: Write('⚖️');
    5: Write('🛒');
    6: Write('🌵');
    7: Write('👮');
    8..13: Write('🏜️');
  end;
  WriteLn;
  WrapWriteLn(S.CurrentRoom^.Description);

  if FindItem('MAP', INV_LOCATION, S) > 0 then begin
    Exits := '';
    if S.CurrentRoom^.North <> nil then Exits := Exits + 'NORTH, ';
    if S.CurrentRoom^.South <> nil then Exits := Exits + 'SOUTH, ';
    if S.CurrentRoom^.East <> nil then Exits := Exits + 'EAST, ';
    if S.CurrentRoom^.West <> nil then Exits := Exits + 'WEST, ';
    if Exits <> '' then begin
      Exits := Copy(Exits, 1, Length(Exits) - 2); // Remove last comma
      WriteLn('🚪 Exits: [', Exits, ']');
    end;
  end;

  if S.SnakeRoom = S.CurrentRoom^.ID then begin
    WriteLn;
    WriteLn('🐍 !!! A RATTLESNAKE is coiled here, buzzing its tail angrily !!!');
    WriteLn('One wrong move could be your last.');
  end;

  if S.OutlawRoom = S.CurrentRoom^.ID then begin
    WriteLn;
    WriteLn('🤠 !!! A DIRTY OUTLAW is leaning against the wall, hand on his holster !!!');
    WriteLn('"You don''t belong here, stranger," he sneers.');
  end;

  FoundItems := False;
  for i := 1 to MAX_ITEMS do
    if S.Items[i].Location = S.CurrentRoom^.ID then begin
      if not FoundItems then begin
        WriteLn;
        WriteLn('📦 You see the following here:');
        FoundItems := True;
      end;
      WriteLn('  - ', S.Items[i].Description);
    end;
  WriteLn;
end;

end.
