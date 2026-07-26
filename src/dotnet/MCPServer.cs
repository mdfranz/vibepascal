namespace Dustwood;

using System.Text.Json;
using System.Text.Json.Nodes;

public class MCPServer
{
    private readonly object _lock = new();
    private Game _game;
    private readonly long? _defaultSeed;
    private readonly int _turnLimit;

    public Game GameInstance
    {
        get
        {
            lock (_lock)
            {
                return _game;
            }
        }
    }

    public MCPServer(long? seed, int turnLimit)
    {
        _defaultSeed = seed;
        _turnLimit = turnLimit;
        _game = new Game(seed, turnLimit, null);
    }

    public static async Task RunAsync(MCPServer server, string addr, string path, List<string> origins, string token, bool jsonResponse, bool stateless)
    {
        var builder = WebApplication.CreateBuilder();

        var uri = new Uri(addr.StartsWith("http://") || addr.StartsWith("https://") ? addr : "http://" + addr);
        builder.WebHost.UseUrls($"http://{uri.Authority}");

        var app = builder.Build();

        if (!path.StartsWith('/')) path = "/" + path;

        app.MapGet("/health", () => Results.Json(new { status = "ok", service = "dustwood-dotnet" }));

        app.MapPost(path, async (HttpContext ctx) =>
        {
            if (!string.IsNullOrEmpty(token))
            {
                var authHeader = ctx.Request.Headers["Authorization"].ToString();
                if (authHeader != $"Bearer {token}")
                {
                    ctx.Response.StatusCode = 401;
                    await ctx.Response.WriteAsync("Unauthorized");
                    return;
                }
            }

            try
            {
                using var doc = await JsonDocument.ParseAsync(ctx.Request.Body);
                var root = doc.RootElement;

                var id = root.TryGetProperty("id", out var idProp) ? idProp.Clone() : (JsonElement?)null;
                string method = root.TryGetProperty("method", out var methodProp) ? methodProp.GetString() ?? "" : "";
                JsonElement paramsElem = root.TryGetProperty("params", out var pElem) ? pElem : default;

                var response = server.HandleJsonRpc(method, paramsElem, id);
                ctx.Response.ContentType = "application/json";
                await ctx.Response.WriteAsync(JsonSerializer.Serialize(response));
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[MCP Error] {ex}");
                ctx.Response.StatusCode = 500;
                await ctx.Response.WriteAsync(JsonSerializer.Serialize(new
                {
                    jsonrpc = "2.0",
                    error = new { code = -32603, message = ex.Message }
                }));
            }
        });

        Console.WriteLine($"[MCP] Listening on http://{uri.Authority}{path}");
        await app.RunAsync();
    }

    public object HandleJsonRpc(string method, JsonElement paramsElem, JsonElement? id)
    {
        object? result = method switch
        {
            "initialize" => HandleInitialize(),
            "notifications/initialized" => new { },
            "tools/list" => HandleToolsList(),
            "tools/call" => HandleToolsCall(paramsElem),
            "resources/list" => HandleResourcesList(),
            "resources/read" => HandleResourcesRead(paramsElem),
            "prompts/list" => HandlePromptsList(),
            "prompts/get" => HandlePromptsGet(paramsElem),
            _ => null
        };

        if (result == null && method != "notifications/initialized")
        {
            return new
            {
                jsonrpc = "2.0",
                id = id,
                error = new { code = -32601, message = $"Method not found: {method}" }
            };
        }

        return new
        {
            jsonrpc = "2.0",
            id = id,
            result = result
        };
    }

    private object HandleInitialize()
    {
        return new
        {
            protocolVersion = "2024-11-05",
            capabilities = new
            {
                tools = new { },
                resources = new { },
                prompts = new { }
            },
            serverInfo = new
            {
                name = "dustwood-dotnet",
                version = "v1.0.0"
            }
        };
    }

    private object HandleToolsList()
    {
        return new
        {
            tools = new object[]
            {
                new
                {
                    name = "command",
                    description = "Send a command to the Dustwood game and return output plus state summary.",
                    inputSchema = new
                    {
                        type = "object",
                        properties = new
                        {
                            command = new { type = "string", description = "Game command to execute" },
                            reset = new { type = "boolean", description = "Reset the game before executing command" },
                            seed = new { type = "integer", description = "Seed to use when resetting game" }
                        }
                    }
                },
                new
                {
                    name = "look",
                    description = "Look around the current room to see exits, items, and description.",
                    inputSchema = new { type = "object", properties = new { } }
                },
                new
                {
                    name = "go",
                    description = "Move in a compass direction (north, south, east, west).",
                    inputSchema = new
                    {
                        type = "object",
                        properties = new
                        {
                            direction = new { type = "string", description = "Compass direction: north, south, east, or west" }
                        },
                        required = new[] { "direction" }
                    }
                },
                new
                {
                    name = "take",
                    description = "Pick up a named item in the current room.",
                    inputSchema = new
                    {
                        type = "object",
                        properties = new
                        {
                            item = new { type = "string", description = "Name of the item to pick up" }
                        },
                        required = new[] { "item" }
                    }
                },
                new
                {
                    name = "drop",
                    description = "Drop an item from your inventory into the current room.",
                    inputSchema = new
                    {
                        type = "object",
                        properties = new
                        {
                            item = new { type = "string", description = "Name of the item to drop" }
                        },
                        required = new[] { "item" }
                    }
                },
                new
                {
                    name = "inventory",
                    description = "List all items you are carrying and view your status.",
                    inputSchema = new { type = "object", properties = new { } }
                },
                new
                {
                    name = "drink",
                    description = "Drink from your canteen to reduce thirst.",
                    inputSchema = new { type = "object", properties = new { } }
                },
                new
                {
                    name = "water_horse",
                    description = "Give water to your horse to reduce its thirst.",
                    inputSchema = new { type = "object", properties = new { } }
                },
                new
                {
                    name = "light",
                    description = "Light your lamp to illuminate dark areas.",
                    inputSchema = new { type = "object", properties = new { } }
                },
                new
                {
                    name = "score",
                    description = "Display your current score and game statistics.",
                    inputSchema = new { type = "object", properties = new { } }
                },
                new
                {
                    name = "reset_game",
                    description = "Start a new game, optionally with a specific seed.",
                    inputSchema = new
                    {
                        type = "object",
                        properties = new
                        {
                            seed = new { type = "integer", description = "Optional seed for deterministic gameplay" }
                        }
                    }
                }
            }
        };
    }

    private object HandleToolsCall(JsonElement paramsElem)
    {
        string name = GetStringProp(paramsElem, "name");
        JsonElement args = (paramsElem.ValueKind == JsonValueKind.Object && paramsElem.TryGetProperty("arguments", out var aProp)) ? aProp : default;

        lock (_lock)
        {
            string outputText = "";
            GameSummary stateSummary;
            bool isError = false;

            switch (name)
            {
                case "command":
                    {
                        string cmd = GetStringProp(args, "command");
                        bool reset = GetBoolProp(args, "reset");
                        long? seed = GetLongProp(args, "seed", _defaultSeed);

                        if (reset)
                        {
                            _game = new Game(seed, _turnLimit, null);
                            (outputText, stateSummary) = ("", SummaryHelper.SummarizeState(_game.State));
                        }
                        else
                        {
                            (outputText, stateSummary) = Game.ExecuteCommand(_game, cmd);
                        }
                        isError = !stateSummary.IsPlaying;
                        break;
                    }
                case "look":
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, "look");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "go":
                    string dir = GetStringProp(args, "direction");
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, dir);
                    isError = !stateSummary.IsPlaying;
                    break;
                case "take":
                    string takeItem = GetStringProp(args, "item");
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, $"take {takeItem}");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "drop":
                    string dropItem = GetStringProp(args, "item");
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, $"drop {dropItem}");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "inventory":
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, "inv");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "drink":
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, "drink");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "water_horse":
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, "water horse");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "light":
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, "light lamp");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "score":
                    (outputText, stateSummary) = Game.ExecuteCommand(_game, "score");
                    isError = !stateSummary.IsPlaying;
                    break;
                case "reset_game":
                    long? rSeed = GetLongProp(args, "seed", _defaultSeed);
                    _game = new Game(rSeed, _turnLimit, null);
                    outputText = "Game reset.";
                    stateSummary = SummaryHelper.SummarizeState(_game.State);
                    isError = !stateSummary.IsPlaying;
                    break;
                default:
                    return new { isError = true, content = new[] { new { type = "text", text = $"Unknown tool: {name}" } } };
            }

            return new
            {
                isError = isError,
                content = new object[]
                {
                    new
                    {
                        type = "text",
                        text = JsonSerializer.Serialize(new
                        {
                            output = outputText,
                            state = stateSummary
                        })
                    }
                }
            };
        }
    }

    private object HandleResourcesList()
    {
        return new
        {
            resources = new object[]
            {
                new { uri = "game://state", name = "Game State", description = "Current game state", mimeType = "application/json" },
                new { uri = "game://room", name = "Current Room", description = "Description of current room", mimeType = "text/plain" },
                new { uri = "game://inventory", name = "Inventory", description = "Player inventory items", mimeType = "text/plain" }
            }
        };
    }

    private object HandleResourcesRead(JsonElement paramsElem)
    {
        string uri = GetStringProp(paramsElem, "uri");
        lock (_lock)
        {
            string contentText = uri switch
            {
                "game://state" => SummaryHelper.SummarizeStateJSON(_game.State),
                "game://room" => SummaryHelper.DescribeRoom(_game.State),
                "game://inventory" => SummaryHelper.DescribeInventory(_game.State),
                _ => ""
            };
            string mimeType = uri == "game://state" ? "application/json" : "text/plain";

            return new
            {
                contents = new[]
                {
                    new { uri = uri, mimeType = mimeType, text = contentText }
                }
            };
        }
    }

    private object HandlePromptsList()
    {
        return new
        {
            prompts = new[]
            {
                new { name = "play", description = "System prompt for LLM agent playing Dustwood." }
            }
        };
    }

    private object HandlePromptsGet(JsonElement paramsElem)
    {
        return new
        {
            description = "Dustwood game-playing instructions",
            messages = new[]
            {
                new
                {
                    role = "user",
                    content = new { type = "text", text = SystemPrompt }
                }
            }
        };
    }

    private static string GetStringProp(JsonElement elem, string propName)
    {
        if (elem.ValueKind == JsonValueKind.Object && elem.TryGetProperty(propName, out var p) && p.ValueKind == JsonValueKind.String)
        {
            return p.GetString() ?? "";
        }
        return "";
    }

    private static bool GetBoolProp(JsonElement elem, string propName)
    {
        if (elem.ValueKind == JsonValueKind.Object && elem.TryGetProperty(propName, out var p) && (p.ValueKind == JsonValueKind.True || p.ValueKind == JsonValueKind.False))
        {
            return p.GetBoolean();
        }
        return false;
    }

    private static long? GetLongProp(JsonElement elem, string propName, long? defaultVal)
    {
        if (elem.ValueKind == JsonValueKind.Object && elem.TryGetProperty(propName, out var p) && p.ValueKind == JsonValueKind.Number)
        {
            return p.GetInt64();
        }
        return defaultVal;
    }

    private const string SystemPrompt = @"You are playing Dustwood, a text-based adventure game set in a dying Western frontier town.
Your goal is to survive, quench your thirst, and escape before nightfall.
Use the command tool to interact with the game.";
}
