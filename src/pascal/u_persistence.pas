unit u_persistence;

{$mode objfpc}{$H+}

interface

uses
  IniFiles, SysUtils, u_types, u_state, u_io;

procedure SaveGame(const S: TGameState; const Path: string);
procedure LoadGame(var S: TGameState; const Path: string);

implementation

procedure SaveGame(const S: TGameState; const Path: string);
var
  Ini: TIniFile;
  i: Integer;
  Section: string;
  RoomsStr, ItemsStr: string;
begin
  Ini := TIniFile.Create(Path);
  try
    Ini.WriteInteger('State', 'CurrentRoom', S.CurrentRoom^.ID);
    Ini.WriteBool('State', 'IsPumpFixed', S.IsPumpFixed);
    Ini.WriteBool('State', 'IsLampLit', S.IsLampLit);
    Ini.WriteBool('State', 'HasWater', S.HasWater);
    Ini.WriteBool('State', 'IsHorseSaddled', S.IsHorseSaddled);
    Ini.WriteBool('State', 'IsRiding', S.IsRiding);
    Ini.WriteBool('State', 'IsTelegraphFixed', S.IsTelegraphFixed);
    Ini.WriteInteger('State', 'TempLightTurns', S.TempLightTurns);
    Ini.WriteInteger('State', 'CanteenDrinks', S.CanteenDrinks);
    Ini.WriteInteger('State', 'Thirst', S.Thirst);
    Ini.WriteInteger('State', 'HorseThirst', S.HorseThirst);
    Ini.WriteInteger('State', 'Turns', S.Turns);
    Ini.WriteInteger('State', 'Score', S.Score);
    for i := 1 to MAX_ITEMS do begin
      Section := 'Item' + IntToStr(i);
      Ini.WriteInteger(Section, 'Location', S.Items[i].Location);
      Ini.WriteString(Section, 'Description', S.Items[i].Description);
    end;
    RoomsStr := '';
    for i := 1 to MAX_ROOMS do
      if S.RoomVisited[i] then RoomsStr := RoomsStr + '1' else RoomsStr := RoomsStr + '0';
    ItemsStr := '';
    for i := 1 to MAX_ITEMS do
      if S.ItemScored[i] then ItemsStr := ItemsStr + '1' else ItemsStr := ItemsStr + '0';
    Ini.WriteString('ScoreFlags', 'RoomVisited', RoomsStr);
    Ini.WriteString('ScoreFlags', 'ItemScored', ItemsStr);
    Ini.WriteBool('ScoreFlags', 'ScoredPumpFix', S.ScoredPumpFix);
    Ini.WriteBool('ScoreFlags', 'ScoredFirstFill', S.ScoredFirstFill);
    Ini.WriteBool('ScoreFlags', 'ScoredLampLight', S.ScoredLampLight);
    Ini.WriteBool('ScoreFlags', 'ScoredBoxOpen', S.ScoredBoxOpen);
    Ini.WriteBool('ScoreFlags', 'ScoredTelegraphFix', S.ScoredTelegraphFix);
    Ini.WriteBool('ScoreFlags', 'ScoredOutlawKill', S.ScoredOutlawKill);
    Ini.WriteBool('ScoreFlags', 'ScoredNoteFound', S.ScoredNoteFound);
    WriteLn('💾 Game saved.');
  finally
    Ini.Free;
  end;
end;

procedure LoadGame(var S: TGameState; const Path: string);
var
  Ini: TIniFile;
  i: Integer;
  Section: string;
  RoomID: Integer;
  RoomsStr, ItemsStr: string;
begin
  if not FileExists(Path) then begin
    WriteLn('No save file found.');
    Exit;
  end;
  Ini := TIniFile.Create(Path);
  try
    RoomID := Ini.ReadInteger('State', 'CurrentRoom', 1);
    S.CurrentRoom := S.RoomRegistry[RoomID];
    S.IsPumpFixed := Ini.ReadBool('State', 'IsPumpFixed', False);
    S.IsLampLit := Ini.ReadBool('State', 'IsLampLit', False);
    S.HasWater := Ini.ReadBool('State', 'HasWater', False);
    S.IsHorseSaddled := Ini.ReadBool('State', 'IsHorseSaddled', False);
    S.IsRiding := Ini.ReadBool('State', 'IsRiding', False);
    S.IsTelegraphFixed := Ini.ReadBool('State', 'IsTelegraphFixed', False);
    S.TempLightTurns := Ini.ReadInteger('State', 'TempLightTurns', 0);
    S.CanteenDrinks := Ini.ReadInteger('State', 'CanteenDrinks', 0);
    S.Thirst := Ini.ReadInteger('State', 'Thirst', 0);
    S.HorseThirst := Ini.ReadInteger('State', 'HorseThirst', 0);
    S.Turns := Ini.ReadInteger('State', 'Turns', 0);
    S.Score := Ini.ReadInteger('State', 'Score', 0);
    for i := 1 to MAX_ITEMS do begin
      Section := 'Item' + IntToStr(i);
      if Ini.SectionExists(Section) then begin
        S.Items[i].Location := Ini.ReadInteger(Section, 'Location', S.Items[i].Location);
        S.Items[i].Description := Ini.ReadString(Section, 'Description', S.Items[i].Description);
      end;
    end;
    RoomsStr := Ini.ReadString('ScoreFlags', 'RoomVisited', '');
    for i := 1 to MAX_ROOMS do begin
      if (Length(RoomsStr) >= i) and (RoomsStr[i] = '1') then S.RoomVisited[i] := True else S.RoomVisited[i] := False;
    end;
    ItemsStr := Ini.ReadString('ScoreFlags', 'ItemScored', '');
    for i := 1 to MAX_ITEMS do begin
      if (Length(ItemsStr) >= i) and (ItemsStr[i] = '1') then S.ItemScored[i] := True else S.ItemScored[i] := False;
    end;
    S.ScoredPumpFix := Ini.ReadBool('ScoreFlags', 'ScoredPumpFix', False);
    S.ScoredFirstFill := Ini.ReadBool('ScoreFlags', 'ScoredFirstFill', False);
    S.ScoredLampLight := Ini.ReadBool('ScoreFlags', 'ScoredLampLight', False);
    S.ScoredBoxOpen := Ini.ReadBool('ScoreFlags', 'ScoredBoxOpen', False);
    S.ScoredTelegraphFix := Ini.ReadBool('ScoreFlags', 'ScoredTelegraphFix', False);
    S.ScoredOutlawKill := Ini.ReadBool('ScoreFlags', 'ScoredOutlawKill', False);
    S.ScoredNoteFound := Ini.ReadBool('ScoreFlags', 'ScoredNoteFound', False);
    if S.IsTelegraphFixed and (S.RoomRegistry[2] <> nil) then
      S.RoomRegistry[2]^.Description := 'The telegraph has been repaired. The line hums faintly with life.';
    WriteLn('📂 Game loaded.');
    Look(S);
  finally
    Ini.Free;
  end;
end;

end.
