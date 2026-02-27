program Dustwood;

{$mode objfpc}{$H+}

uses
  SysUtils, u_types, u_state, u_world, u_io, u_commands;

var
  State: TGameState;
  TurnLimit: Integer;
  i, Seed: Integer;
  Arg: string;

procedure PrintHelp;
begin
  WriteLn('Usage: dustwood [options]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -h, --h, --help Show this help message');
  WriteLn('  --headless      Run in headless mode');
  WriteLn('  --turns <n>     Set the turn limit (default: 50)');
  WriteLn('  --seed <n>      Set the random seed');
  Halt(0);
end;

begin
  TurnLimit := 50;
  Seed := -1;
  InitState(State);

  i := 1;
  while i <= ParamCount do begin
    Arg := ParamStr(i);
    if (Arg = '-h') or (Arg = '--h') or (Arg = '--help') then
      PrintHelp
    else if Arg = '--headless' then
      State.IsHeadless := True
    else if Arg = '--turns' then begin
      Inc(i);
      if i <= ParamCount then TurnLimit := StrToIntDef(ParamStr(i), 50);
    end else if Arg = '--seed' then begin
      Inc(i);
      if i <= ParamCount then Seed := StrToIntDef(ParamStr(i), -1);
    end;
    Inc(i);
  end;

  LoadWorld(State, 'data/world.ini');
  if Seed <> -1 then
    RandSeed := Seed
  else
    Randomize;

  RandomizeMapLocation(State);
  State.CurrentRoom := State.RoomRegistry[1];
  State.IsPlaying := True;
  Look(State);
  while State.IsPlaying do begin
    ProcessCommand(State, CustomReadLn(State, '> '));
    if (State.Turns >= TurnLimit) and State.IsPlaying then begin
      WriteLn;
      WriteLn('⏳ You have taken too long. The sun dips below the horizon.');
      WriteLn('GAME OVER.');
      State.IsPlaying := False;
    end;
  end;
  WriteLn;
  WriteLn('🏆 Final score: ', State.Score);
end.
