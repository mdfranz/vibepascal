program Dustwood;

{$mode objfpc}{$H+}

uses
  SysUtils, u_types, u_state, u_world, u_io, u_commands;

var
  State: TGameState;
  TurnLimit: Integer;

begin
  TurnLimit := 50;
  InitState(State);
  State.IsHeadless := (ParamCount > 0) and (ParamStr(1) = '--headless');
  LoadWorld(State, 'data/world.ini');
  Randomize;
  RandomizeMapLocation(State);
  State.CurrentRoom := State.RoomRegistry[1];
  State.IsPlaying := True;
  Look(State);
  while State.IsPlaying do
    ProcessCommand(State, CustomReadLn(State, '> '));
    if (State.Turns >= TurnLimit) and State.IsPlaying then begin
      WriteLn;
      WriteLn('⏳ You have taken too long. The sun dips below the horizon.');
      WriteLn('GAME OVER.');
      State.IsPlaying := False;
    end;
  WriteLn;
  WriteLn('🏆 Final score: ', State.Score);
end.
