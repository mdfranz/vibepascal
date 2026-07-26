namespace Dustwood;

public class Program
{
    public static async Task Main(string[] args)
    {
        bool headless = false;
        bool mcpHttp = false;
        string mcpAddr = "127.0.0.1:8765";
        string mcpPath = "/mcp";
        string mcpToken = "";
        bool mcpJsonResponse = false;
        bool mcpStateless = false;
        long? seed = null;
        int turnLimit = 25;
        bool autosaveEnabled = false;
        int autosaveInterval = 5;
        string autosavePath = "data/autosave.db";
        var origins = new List<string>();

        for (int i = 0; i < args.Length; i++)
        {
            string arg = args[i];
            if (arg == "--headless") headless = true;
            else if (arg == "--mcp-http") mcpHttp = true;
            else if (arg == "--mcp-json-response") mcpJsonResponse = true;
            else if (arg == "--mcp-stateless") mcpStateless = true;
            else if (arg == "--autosave") autosaveEnabled = true;
            else if (arg == "--turns" && i + 1 < args.Length) turnLimit = int.Parse(args[++i]);
            else if (arg == "--seed" && i + 1 < args.Length) seed = long.Parse(args[++i]);
            else if (arg == "--mcp-addr" && i + 1 < args.Length) mcpAddr = args[++i];
            else if (arg == "--mcp-path" && i + 1 < args.Length) mcpPath = args[++i];
            else if (arg == "--mcp-token" && i + 1 < args.Length) mcpToken = args[++i];
            else if (arg == "--mcp-origin" && i + 1 < args.Length) origins.Add(args[++i]);
            else if (arg == "--autosave-interval" && i + 1 < args.Length) autosaveInterval = int.Parse(args[++i]);
            else if (arg == "--autosave-path" && i + 1 < args.Length) autosavePath = args[++i];
            else if (arg == "-h" || arg == "--help" || arg == "-help")
            {
                ShowHelp();
                return;
            }
        }

        if (mcpHttp)
        {
            if (origins.Count == 0)
            {
                origins.Add("http://localhost");
                origins.Add("http://127.0.0.1");
            }

            var server = new MCPServer(seed, turnLimit);
            server.GameInstance.State.AutosaveEnabled = autosaveEnabled;
            server.GameInstance.State.AutosaveInterval = autosaveInterval;
            server.GameInstance.State.AutosavePath = autosavePath;

            await MCPServer.RunAsync(server, mcpAddr, mcpPath, origins, mcpToken, mcpJsonResponse, mcpStateless);
            return;
        }

        var game = new Game(seed, turnLimit, null);
        game.State.IsHeadless = headless;
        game.State.AutosaveEnabled = autosaveEnabled;
        game.State.AutosaveInterval = autosaveInterval;
        game.State.AutosavePath = autosavePath;

        while (game.State.IsPlaying)
        {
            string input = IO.CustomReadLn(game.State, "> ");
            Commands.ProcessCommand(game.State, input, game.Rng);
        }

        IO.OutPrintln(game.State);
        IO.OutPrintf(game.State, "🏆 Final score: {0}\n", game.State.Score);
    }

    private static void ShowHelp()
    {
        Console.WriteLine("Usage: dustwood-dotnet [options]\n");
        Console.WriteLine("Options:");
        Console.WriteLine("  -h, --help           Show this help message");
        Console.WriteLine("  --headless           Run in headless mode");
        Console.WriteLine("  --turns <n>          Set the turn limit (default: 25)");
        Console.WriteLine("  --seed <n>           Set the random seed");
        Console.WriteLine("  --autosave           Enable autosave feature");
        Console.WriteLine("  --autosave-interval  Turns between autosaves (default: 5)");
        Console.WriteLine("  --autosave-path      Autosave file path (default: data/autosave.db)");
        Console.WriteLine("  --mcp-http           Run MCP Streamable HTTP server");
        Console.WriteLine("  --mcp-addr <addr>    MCP listen address (default: 127.0.0.1:8765)");
        Console.WriteLine("  --mcp-path <path>    MCP endpoint path (default: /mcp)");
        Console.WriteLine("  --mcp-token <tok>    Bearer token for MCP requests (optional)");
        Console.WriteLine("  --mcp-json-response  Force JSON responses instead of SSE");
        Console.WriteLine("  --mcp-stateless      Run MCP server in stateless mode");
        Console.WriteLine("  --mcp-origin <orig>  Allowed Origin for MCP requests (repeatable)");
    }
}
