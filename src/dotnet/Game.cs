namespace Dustwood;

public class Game
{
    public GameState State { get; private set; }
    public Random Rng { get; private set; }

    public Game(long? seed, int turnLimit, TextWriter? outWriter)
    {
        State = new GameState();
        State.TurnLimit = turnLimit;
        if (outWriter != null)
        {
            State.Out = outWriter;
        }

        World.LoadWorld(State, "data/world.ini");

        if (seed.HasValue)
        {
            Rng = new Random((int)seed.Value);
        }
        else
        {
            Rng = new Random();
        }

        World.RandomizeMapLocation(State, Rng);
        State.CurrentRoom = State.RoomRegistry[1];
        State.IsPlaying = true;

        IO.Look(State);
    }

    public static (string Output, GameSummary State) ExecuteCommand(Game game, string cmd)
    {
        using var sw = new StringWriter();
        var prevOut = game.State.Out;
        game.State.Out = sw;

        try
        {
            string trimmed = cmd.Trim();
            if (string.IsNullOrEmpty(trimmed))
            {
                IO.Look(game.State);
            }
            else
            {
                Commands.ProcessCommand(game.State, trimmed, game.Rng);
            }

            if (game.State.IsPlaying && game.State.TurnLimit > 0 && game.State.Turns >= game.State.TurnLimit)
            {
                IO.OutPrintln(game.State);
                IO.OutPrintln(game.State, "⏳ You have taken too long. The sun dips below the horizon.");
                IO.OutPrintln(game.State, "GAME OVER.");
                game.State.IsPlaying = false;
            }
        }
        finally
        {
            game.State.Out = prevOut;
        }

        return (sw.ToString(), SummaryHelper.SummarizeState(game.State));
    }
}
