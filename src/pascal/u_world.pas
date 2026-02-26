unit u_world;

{$mode objfpc}{$H+}

interface

uses
  IniFiles, SysUtils, u_types, u_state;

procedure LoadWorld(var S: TGameState; const Path: string);
procedure RandomizeMapLocation(var S: TGameState);

implementation

procedure RandomizeMapLocation(var S: TGameState);
var
  i: Integer;
  MapIdx: Integer;
  NewLoc: Integer;
begin
  MapIdx := 0;
  for i := 1 to MAX_ITEMS do
    if S.Items[i].Name = 'MAP' then begin
      MapIdx := i;
      Break;
    end;

  if MapIdx > 0 then begin
    // Random town room between 1 and 7
    NewLoc := Random(7) + 1;
    S.Items[MapIdx].Location := NewLoc;
  end;
end;

procedure LoadWorld(var S: TGameState; const Path: string);
var
  i: Integer;
  Ini: TIniFile;
  Section: string;
  N, SouthId, EastId, WestId: Integer;
begin
  Ini := TIniFile.Create(Path);
  try
    for i := 1 to MAX_ROOMS do begin
      Section := 'Room' + IntToStr(i);
      if Ini.SectionExists(Section) then begin
        New(S.RoomRegistry[i]);
        S.RoomRegistry[i]^.ID := i;
        S.RoomRegistry[i]^.Name := Ini.ReadString(Section, 'Name', '');
        S.RoomRegistry[i]^.Description := Ini.ReadString(Section, 'Description', '');
        S.RoomRegistry[i]^.North := nil;
        S.RoomRegistry[i]^.South := nil;
        S.RoomRegistry[i]^.East := nil;
        S.RoomRegistry[i]^.West := nil;
      end;
    end;
    for i := 1 to MAX_ROOMS do if S.RoomRegistry[i] <> nil then begin
      Section := 'Room' + IntToStr(i);
      N := Ini.ReadInteger(Section, 'North', 0);
      SouthId := Ini.ReadInteger(Section, 'South', 0);
      EastId := Ini.ReadInteger(Section, 'East', 0);
      WestId := Ini.ReadInteger(Section, 'West', 0);
      if N > 0 then S.RoomRegistry[i]^.North := S.RoomRegistry[N];
      if SouthId > 0 then S.RoomRegistry[i]^.South := S.RoomRegistry[SouthId];
      if EastId > 0 then S.RoomRegistry[i]^.East := S.RoomRegistry[EastId];
      if WestId > 0 then S.RoomRegistry[i]^.West := S.RoomRegistry[WestId];
    end;
    for i := 1 to MAX_ITEMS do begin
      Section := 'Item' + IntToStr(i);
      if Ini.SectionExists(Section) then begin
        S.Items[i].Name := UpperCase(Ini.ReadString(Section, 'Name', ''));
        S.Items[i].Description := Ini.ReadString(Section, 'Description', '');
        S.Items[i].Details := Ini.ReadString(Section, 'Details', '');
        S.Items[i].Location := Ini.ReadInteger(Section, 'Location', 0);
        S.Items[i].IsTakeable := Ini.ReadInteger(Section, 'IsTakeable', 0) = 1;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

end.
