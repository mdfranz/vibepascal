program Dustwood;

{$mode objfpc}{$H+}

uses
  SysUtils, u_types, u_state, u_world, u_io, u_commands;

var
  State: TGameState;

begin
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
  WriteLn;
  WriteLn('🏆 Final score: ', State.Score);
end.
