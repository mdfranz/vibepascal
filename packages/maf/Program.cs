using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using Microsoft.Agents.AI;
using Microsoft.Extensions.AI;
using ModelContextProtocol.Client;

namespace MafMcpClient;

public class Program
{
    public record GameState(
        string RoomName,
        int Turns,
        int Score,
        int Thirst,
        bool IsPlaying
    )
    {
        public static GameState FromJson(JsonElement elem)
        {
            string room = "Unknown";
            if (elem.TryGetProperty("room_name", out var r1) && r1.ValueKind == JsonValueKind.String) room = r1.GetString()!;
            else if (elem.TryGetProperty("roomName", out var r2) && r2.ValueKind == JsonValueKind.String) room = r2.GetString()!;

            int turns = 0;
            if (elem.TryGetProperty("turns", out var t) && t.ValueKind == JsonValueKind.Number) turns = t.GetInt32();

            int score = 0;
            if (elem.TryGetProperty("score", out var s) && s.ValueKind == JsonValueKind.Number) score = s.GetInt32();

            int thirst = 0;
            if (elem.TryGetProperty("thirst", out var th) && th.ValueKind == JsonValueKind.Number) thirst = th.GetInt32();

            bool isPlaying = true;
            if (elem.TryGetProperty("is_playing", out var ip1) && (ip1.ValueKind == JsonValueKind.True || ip1.ValueKind == JsonValueKind.False)) isPlaying = ip1.GetBoolean();
            else if (elem.TryGetProperty("isPlaying", out var ip2) && (ip2.ValueKind == JsonValueKind.True || ip2.ValueKind == JsonValueKind.False)) isPlaying = ip2.GetBoolean();

            return new GameState(room, turns, score, thirst, isPlaying);
        }
    }

    public static async Task<int> Main(string[] args)
    {
        LoadDotEnv();

        string mcpUrl = Environment.GetEnvironmentVariable("MCP_URL") ?? "http://127.0.0.1:8765/mcp";

        // Parse CLI arguments
        string level = args.Length > 0 ? args[0] : "full";
        string rawModel = args.Length > 1 ? args[1] : "google/gemini-2.5-flash";
        int delay = args.Length > 2 && int.TryParse(args[2], out var d) ? d : 1;
        int maxTurns = args.Length > 3 && int.TryParse(args[3], out var mt) ? mt : 25;

        bool summarize = false;
        bool windowing = false;
        int windowSize = 6;
        string? sessionId = null;

        for (int i = 4; i < args.Length; i++)
        {
            string arg = args[i];
            if (arg == "--summarize" || arg == "-s") summarize = true;
            else if (arg == "--windowing" || arg == "-w") windowing = true;
            else if ((arg == "--window-size" || arg == "-n") && i + 1 < args.Length)
            {
                if (int.TryParse(args[++i], out var ws)) windowSize = ws;
            }
            else if (arg == "--session-id" && i + 1 < args.Length)
            {
                sessionId = args[++i];
            }
        }

        long epoch = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
        Directory.CreateDirectory("logs");
        Directory.CreateDirectory("sessions/maf_sessions");
        string logFile = $"logs/maf_mcp_client-{epoch}.log";

        void LogKv(string evt, Dictionary<string, object?> kv)
        {
            var sb = new StringBuilder();
            sb.Append($"event={evt}");
            foreach (var (k, v) in kv)
            {
                if (v == null) continue;
                string valStr = v is string s ? FormatStringVal(s) : v.ToString() ?? "";
                sb.Append($" {k}={valStr}");
            }
            string line = sb.ToString();
            File.AppendAllText(logFile, line + "\n");
            
            bool gameConsole = Environment.GetEnvironmentVariable("GAME_CONSOLE") != "0";
            if (gameConsole)
            {
                Console.WriteLine($"[{evt}] {line}");
            }
        }

        string FormatStringVal(string str)
        {
            if (str.Contains(' ') || str.Contains('\n') || str.Contains('"') || str.Contains('='))
            {
                string escaped = str.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\r\n", "\\n").Replace("\n", "\\n");
                return $"\"{escaped}\"";
            }
            return str;
        }

        LogKv("client_start", new() {
            ["framework"] = "Microsoft.Agents.AI (.NET OpenRouter)",
            ["model"] = rawModel,
            ["level"] = level,
            ["delay"] = delay,
            ["max_turns"] = maxTurns,
            ["log_file"] = logFile,
            ["summarize"] = summarize,
            ["windowing"] = windowing,
            ["window_size"] = windowSize
        });

        // Load guidance
        string guidanceText = LoadGuidance(level);
        string guidanceBlock = string.IsNullOrWhiteSpace(guidanceText) ? "" : $"\n\nGUIDANCE (follow this):\n{guidanceText}";

        // Configure OpenRouter IChatClient
        IChatClient chatClient = CreateOpenRouterChatClient(rawModel);

        // System prompt
        string systemPrompt = "You are an expert adventurer playing 'Echoes of Dustwood' via an MCP interface.\n" +
            "Use the available MCP tools to play the game.\n" +
            "Start with LOOK to see your surroundings.\n" +
            "LOOK does not consume a game turn; do not repeat LOOK if turns did not change.\n" +
            "Try to explore, find items, solve puzzles, and survive as long as possible.\n" +
            "Always invoke the 'command' tool with your next game action." + guidanceBlock;

        // Connect to MCP Server
        Console.WriteLine($"Connecting to MCP server at {mcpUrl}...");
        var transport = new HttpClientTransport(new HttpClientTransportOptions
        {
            Endpoint = new Uri(mcpUrl)
        });

        await using var mcpClient = await McpClient.CreateAsync(transport);

        var mcpTools = await mcpClient.ListToolsAsync();
        Console.WriteLine($"Discovered {mcpTools.Count} tools from MCP server.");

        var aiTools = mcpTools.Cast<AITool>().ToList();

        // Create ChatClientAgent using Microsoft.Agents.AI
        var agentOptions = new ChatClientAgentOptions
        {
            Name = "DustwoodAgent",
            Description = "Text adventure agent powered by Microsoft Agent Framework via OpenRouter",
            ChatOptions = new ChatOptions
            {
                Instructions = systemPrompt,
                Tools = aiTools
            }
        };

        var agent = new ChatClientAgent(chatClient, agentOptions);

        // Loop variables
        GameState? lastState = null;
        string lastOutput = "";
        int currentTurn = 0;
        int totalInputTokens = 0;
        int totalOutputTokens = 0;
        int totalTokens = 0;
        double totalLatencyMs = 0;
        string stopReason = "completed";

        // Session recovery
        string activeSessionId = sessionId ?? $"maf-session-{epoch}";
        string sessionPath = Path.Combine("sessions/maf_sessions", $"{activeSessionId}.json");

        if (!string.IsNullOrEmpty(sessionId) && File.Exists(sessionPath))
        {
            try
            {
                string json = File.ReadAllText(sessionPath);
                Console.WriteLine($"Loaded previous session snapshot from {sessionPath}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to load session snapshot: {ex.Message}");
            }
        }

        string prompt = "Start game. Issue LOOK to inspect your starting location.";
        int turnCount = 0;
        bool isPlaying = true;

        while (isPlaying && currentTurn < maxTurns && turnCount < maxTurns * 3)
        {
            turnCount++;
            var stopwatch = Stopwatch.StartNew();

            AgentResponse response;
            try
            {
                response = await agent.RunAsync(prompt);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Agent execution error: {ex.Message}");
                stopReason = $"error: {ex.Message}";
                break;
            }

            stopwatch.Stop();
            double latencyMs = stopwatch.Elapsed.TotalMilliseconds;
            totalLatencyMs += latencyMs;

            // Usage extraction
            int inputTokens = (int)(response.Usage?.InputTokenCount ?? 0);
            int outputTokens = (int)(response.Usage?.OutputTokenCount ?? 0);
            int callTokens = inputTokens + outputTokens;

            totalInputTokens += inputTokens;
            totalOutputTokens += outputTokens;
            totalTokens += callTokens;

            LogKv("provider_call", new() {
                ["model"] = rawModel,
                ["input_tokens"] = inputTokens,
                ["output_tokens"] = outputTokens,
                ["total_tokens"] = callTokens,
                ["latency_ms"] = Math.Round(latencyMs, 2)
            });

            // Inspect messages and tool results from response
            if (response.Messages != null)
            {
                foreach (var msg in response.Messages)
                {
                    foreach (var contents in msg.Contents)
                    {
                        if (contents is FunctionCallContent call)
                        {
                            string cmd = "";
                            if (call.Arguments != null && call.Arguments.TryGetValue("command", out var cObj))
                            {
                                cmd = cObj?.ToString() ?? "";
                            }
                            LogKv("tool_call", new() {
                                ["tool"] = call.Name,
                                ["command"] = cmd
                            });
                        }
                        else if (contents is FunctionResultContent result)
                        {
                            if (result.Result is JsonElement elem)
                            {
                                ParseStructuredContent(elem, ref lastState, ref lastOutput);
                            }
                            else if (result.Result != null)
                            {
                                try
                                {
                                    string resJson = JsonSerializer.Serialize(result.Result);
                                    using var doc = JsonDocument.Parse(resJson);
                                    ParseStructuredContent(doc.RootElement, ref lastState, ref lastOutput);
                                }
                                catch { }
                            }
                        }
                    }
                }
            }

            if (lastState != null)
            {
                currentTurn = lastState.Turns;
                isPlaying = lastState.IsPlaying;
            }

            prompt = $"Current turn: {currentTurn}, Room: {lastState?.RoomName ?? "Unknown"}. Continue game.";

            if (delay > 0)
            {
                await Task.Delay(delay * 1000);
            }
        }

        if (currentTurn >= maxTurns) stopReason = "max_turns_reached";
        else if (lastState != null && !lastState.IsPlaying) stopReason = "game_over";

        bool win = lastState != null && lastState.Score >= 100;
        bool loss = lastState != null && (!lastState.IsPlaying && !win);

        LogKv("run_summary", new() {
            ["turns"] = currentTurn,
            ["total_turns"] = turnCount,
            ["final_score"] = lastState?.Score ?? 0,
            ["final_thirst"] = lastState?.Thirst ?? 0,
            ["room"] = lastState?.RoomName ?? "Unknown",
            ["win"] = win,
            ["loss"] = loss,
            ["stop_reason"] = stopReason,
            ["total_input_tokens"] = totalInputTokens,
            ["total_output_tokens"] = totalOutputTokens,
            ["total_tokens"] = totalTokens,
            ["total_latency_ms"] = Math.Round(totalLatencyMs, 2)
        });

        // Save session state
        try
        {
            var sessionSnapshot = new
            {
                session_id = activeSessionId,
                turns = currentTurn,
                score = lastState?.Score ?? 0,
                room = lastState?.RoomName ?? "Unknown"
            };
            File.WriteAllText(sessionPath, JsonSerializer.Serialize(sessionSnapshot, new JsonSerializerOptions { WriteIndented = true }));
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Failed to save session snapshot: {ex.Message}");
        }

        Console.WriteLine("--- MAF OpenRouter MCP Session Complete ---");
        return 0;
    }

    private static void ParseStructuredContent(JsonElement root, ref GameState? state, ref string output)
    {
        JsonElement target = root;
        if (root.TryGetProperty("structuredContent", out var sc) && sc.ValueKind == JsonValueKind.Object)
        {
            target = sc;
        }

        if (target.TryGetProperty("output", out var outElem) && outElem.ValueKind == JsonValueKind.String)
        {
            output = outElem.GetString() ?? "";
        }

        if (target.TryGetProperty("state", out var stateElem) && stateElem.ValueKind == JsonValueKind.Object)
        {
            state = GameState.FromJson(stateElem);
        }
    }

    private static string LoadGuidance(string level)
    {
        string path = level.ToLower() switch
        {
            "minimal" => "data/guidance_minimal.txt",
            "medium" => "data/guidance_medium.txt",
            _ => "data/guidance_full.txt"
        };

        if (File.Exists(path))
        {
            return File.ReadAllText(path).Trim();
        }
        return "";
    }

    private static IChatClient CreateOpenRouterChatClient(string modelName)
    {
        string apiKey = Environment.GetEnvironmentVariable("OPENROUTER_API_KEY")
            ?? Environment.GetEnvironmentVariable("OPENAI_API_KEY")
            ?? "";

        if (string.IsNullOrWhiteSpace(apiKey))
        {
            apiKey = PromptForApiKey();
            if (string.IsNullOrWhiteSpace(apiKey))
            {
                throw new InvalidOperationException("OPENROUTER_API_KEY is required to proceed.");
            }
            Environment.SetEnvironmentVariable("OPENROUTER_API_KEY", apiKey);
        }

        string endpoint = Environment.GetEnvironmentVariable("OPENROUTER_BASE_URL") ?? "https://openrouter.ai/api/v1";

        var options = new OpenAI.OpenAIClientOptions
        {
            Endpoint = new Uri(endpoint)
        };

        var client = new OpenAI.OpenAIClient(new System.ClientModel.ApiKeyCredential(apiKey), options);
        return client.GetChatClient(modelName).AsIChatClient();
    }

    private static string PromptForApiKey()
    {
        Console.Write("OPENROUTER_API_KEY is not set. Please enter your OpenRouter API Key: ");
        if (Console.IsInputRedirected)
        {
            return Console.ReadLine()?.Trim() ?? "";
        }

        var sb = new StringBuilder();
        while (true)
        {
            var keyInfo = Console.ReadKey(intercept: true);
            if (keyInfo.Key == ConsoleKey.Enter)
            {
                Console.WriteLine();
                break;
            }
            if (keyInfo.Key == ConsoleKey.Backspace)
            {
                if (sb.Length > 0)
                {
                    sb.Length--;
                    Console.Write("\b \b");
                }
            }
            else if (!char.IsControl(keyInfo.KeyChar))
            {
                sb.Append(keyInfo.KeyChar);
                Console.Write("*");
            }
        }
        return sb.ToString().Trim();
    }

    private static void LoadDotEnv()
    {
        string current = Directory.GetCurrentDirectory();
        while (!string.IsNullOrEmpty(current))
        {
            string envPath = Path.Combine(current, ".env");
            if (File.Exists(envPath))
            {
                foreach (var line in File.ReadAllLines(envPath))
                {
                    string trimmed = line.Trim();
                    if (string.IsNullOrWhiteSpace(trimmed) || trimmed.StartsWith('#')) continue;
                    int idx = trimmed.IndexOf('=');
                    if (idx > 0)
                    {
                        string key = trimmed.Substring(0, idx).Trim();
                        string val = trimmed.Substring(idx + 1).Trim().Trim('"', '\'');
                        if (string.IsNullOrEmpty(Environment.GetEnvironmentVariable(key)))
                        {
                            Environment.SetEnvironmentVariable(key, val);
                        }
                    }
                }
                break;
            }
            var parent = Directory.GetParent(current);
            if (parent == null) break;
            current = parent.FullName;
        }
    }
}
