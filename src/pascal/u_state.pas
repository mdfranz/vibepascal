unit u_state;

{$mode objfpc}{$H+}

interface

uses
  u_types;

type
  TGameState = record
    RoomRegistry: array[1..MAX_ROOMS] of PRoom;
    Items: array[1..MAX_ITEMS] of TItem;
    CurrentRoom: PRoom;
    IsPlaying: Boolean;
    IsPumpFixed: Boolean;
    IsLampLit: Boolean;
    HasWater: Boolean;
    IsHeadless: Boolean;
    IsBoxOpen: Boolean;
    IsTelegraphFixed: Boolean;
    TempLightTurns: Integer;
    CanteenDrinks: Integer;
    IsHorseSaddled: Boolean;
    IsRiding: Boolean;
    SnakeRoom: Integer;
    OutlawRoom: Integer;
    Thirst: Integer;
    Turns: Integer;
    HorseThirst: Integer;
    Score: Integer;
    RoomVisited: array[1..MAX_ROOMS] of Boolean;
    ItemScored: array[1..MAX_ITEMS] of Boolean;
    ScoredPumpFix: Boolean;
    ScoredFirstFill: Boolean;
    ScoredLampLight: Boolean;
    ScoredBoxOpen: Boolean;
    ScoredTelegraphFix: Boolean;
    ScoredOutlawKill: Boolean;
    ScoredNoteFound: Boolean;
    History: array[0..MAX_HISTORY] of string;
    HistoryCount: Integer;
  end;

procedure InitState(var S: TGameState);

implementation

procedure InitState(var S: TGameState);
var
  i: Integer;
begin
  for i := 1 to MAX_ROOMS do S.RoomRegistry[i] := nil;
  for i := 1 to MAX_ITEMS do begin
    S.Items[i].Name := '';
    S.Items[i].Description := '';
    S.Items[i].Details := '';
    S.Items[i].Location := 0;
    S.Items[i].IsTakeable := False;
  end;
  S.CurrentRoom := nil;
  S.IsPlaying := True;
  S.IsPumpFixed := False;
  S.IsLampLit := False;
  S.HasWater := False;
  S.IsHeadless := False;
  S.IsBoxOpen := False;
  S.IsTelegraphFixed := False;
  S.TempLightTurns := 0;
  S.CanteenDrinks := 0;
  S.IsHorseSaddled := False;
  S.IsRiding := False;
  S.SnakeRoom := 0;
  S.OutlawRoom := 0;
  S.Thirst := 0;
  S.Turns := 0;
  S.HorseThirst := 0;
  S.Score := 0;
  for i := 1 to MAX_ROOMS do S.RoomVisited[i] := False;
  for i := 1 to MAX_ITEMS do S.ItemScored[i] := False;
  S.ScoredPumpFix := False;
  S.ScoredFirstFill := False;
  S.ScoredLampLight := False;
  S.ScoredBoxOpen := False;
  S.ScoredTelegraphFix := False;
  S.ScoredOutlawKill := False;
  S.ScoredNoteFound := False;
  for i := 0 to MAX_HISTORY do S.History[i] := '';
  S.HistoryCount := 0;
end;

end.
