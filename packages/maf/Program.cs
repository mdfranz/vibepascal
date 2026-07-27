using System;
using System.ClientModel.Primitives;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Agents.AI;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using ModelContextProtocol.Client;
using OpenTelemetry;
using OpenTelemetry.Logs;
using OpenTelemetry.Resources;
using OpenTelemetry.Trace;

namespace MafMcpClient;

public class Program
{
    private const string ActivitySourceName = "MafMcpClient.Dustwood";
    private static readonly ActivitySource ActivitySource = new(ActivitySourceName);

    private static readonly StringBuilder ReasoningSink = new();

    private static void OnReasoningChunk(string text)
    {
        lock (ReasoningSink) { ReasoningSink.Append(text); }
    }

    private static string FlushReasoningSink()
    {
        lock (ReasoningSink)
        {
            string s = ReasoningSink.ToString();
            ReasoningSink.Clear();
            return s;
        }
    }

    // OpenRouter streams reasoning tokens as a non-standard "reasoning" field on each SSE
    // delta, alongside "content". The official OpenAI .NET SDK types Microsoft.Extensions.AI.OpenAI
    // deserializes against have no property for it, so it's silently dropped before it ever
    // reaches TextReasoningContent. This policy re-reads the raw SSE bytes off the wire to
    // recover it, without disturbing the stream the SDK itself consumes.
    private sealed class ReasoningSniffingPolicy : PipelinePolicy
    {
        public override void Process(PipelineMessage message, IReadOnlyList<PipelinePolicy> pipeline, int index)
        {
            ProcessNext(message, pipeline, index);
            WrapStream(message);
        }

        public override async ValueTask ProcessAsync(PipelineMessage message, IReadOnlyList<PipelinePolicy> pipeline, int index)
        {
            await ProcessNextAsync(message, pipeline, index);
            WrapStream(message);
        }

        private static void WrapStream(PipelineMessage message)
        {
            var stream = message.Response?.ContentStream;
            if (stream != null && stream is not ReasoningSniffingStream)
            {
                message.Response!.ContentStream = new ReasoningSniffingStream(stream);
            }
        }
    }

    private sealed class ReasoningSniffingStream : Stream
    {
        private readonly Stream _inner;
        private readonly StringBuilder _lineBuffer = new();

        public ReasoningSniffingStream(Stream inner) => _inner = inner;

        public override bool CanRead => true;
        public override bool CanSeek => false;
        public override bool CanWrite => false;
        public override long Length => throw new NotSupportedException();
        public override long Position
        {
            get => throw new NotSupportedException();
            set => throw new NotSupportedException();
        }

        public override void Flush() => _inner.Flush();
        public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();
        public override void SetLength(long value) => throw new NotSupportedException();
        public override void Write(byte[] buffer, int offset, int count) => throw new NotSupportedException();

        public override int Read(byte[] buffer, int offset, int count)
        {
            int n = _inner.Read(buffer, offset, count);
            if (n > 0) Sniff(buffer, offset, n);
            return n;
        }

        public override async Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken)
        {
            int n = await _inner.ReadAsync(buffer, offset, count, cancellationToken);
            if (n > 0) Sniff(buffer, offset, n);
            return n;
        }

        private void Sniff(byte[] buffer, int offset, int count)
        {
            _lineBuffer.Append(Encoding.UTF8.GetString(buffer, offset, count));
            string all = _lineBuffer.ToString();
            int lastNewline = all.LastIndexOf('\n');
            if (lastNewline < 0) return;

            string complete = all[..lastNewline];
            _lineBuffer.Clear();
            _lineBuffer.Append(all[(lastNewline + 1)..]);

            foreach (var rawLine in complete.Split('\n'))
            {
                string line = rawLine.TrimEnd('\r');
                if (!line.StartsWith("data: ")) continue;
                string payload = line["data: ".Length..].Trim();
                if (payload.Length == 0 || payload == "[DONE]") continue;

                try
                {
                    using var doc = JsonDocument.Parse(payload);
                    if (doc.RootElement.TryGetProperty("choices", out var choices) && choices.GetArrayLength() > 0)
                    {
                        var delta = choices[0].GetProperty("delta");
                        if (delta.TryGetProperty("reasoning", out var r) && r.ValueKind == JsonValueKind.String)
                        {
                            string text = r.GetString() ?? "";
                            if (text.Length > 0) OnReasoningChunk(text);
                        }
                    }
                }
                catch { }
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) _inner.Dispose();
            base.Dispose(disposing);
        }
    }

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

        var resourceBuilder = ResourceBuilder.CreateDefault().AddService("maf-dustwood-client");

        using var tracerProvider = Sdk.CreateTracerProviderBuilder()
            .SetResourceBuilder(resourceBuilder)
            .AddSource(ActivitySourceName)
            .AddOtlpExporter()
            .Build();

        // Without this, LogKv only wrote to the local file/console - none of it reached Logfire.
        // Emitting through ILogger while an Activity is active correlates each record to its
        // trace/span automatically via the ambient Activity.Current.
        using var loggerFactory = LoggerFactory.Create(logging =>
        {
            logging.AddOpenTelemetry(options =>
            {
                options.SetResourceBuilder(resourceBuilder);
                options.IncludeFormattedMessage = true;
                options.AddOtlpExporter();
            });
        });
        ILogger gameLogger = loggerFactory.CreateLogger(ActivitySourceName);

        string mcpUrl = Environment.GetEnvironmentVariable("MCP_URL") ?? "http://127.0.0.1:8765/mcp";

        // Parse CLI arguments
        string level = args.Length > 0 ? args[0] : "full";
        string rawModel = args.Length > 1 ? args[1] : "google/gemini-3.5-flash";
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

            var fields = kv.Where(p => p.Value != null).ToList();
            string template = "dustwood {Event}" + string.Concat(fields.Select(p => $" {p.Key}={{{p.Key}}}"));
            object?[] args = new object?[] { evt }.Concat(fields.Select(p => p.Value)).ToArray();
            gameLogger.LogInformation(template, args);
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

        using var sessionActivity = ActivitySource.StartActivity("maf.session", ActivityKind.Client);
        sessionActivity?.SetTag("gen_ai.operation.name", "invoke_agent");
        sessionActivity?.SetTag("gen_ai.agent.name", "DustwoodAgent");
        sessionActivity?.SetTag("gen_ai.provider.name", "openrouter");
        sessionActivity?.SetTag("gen_ai.request.model", rawModel);
        sessionActivity?.SetTag("dustwood.level", level);
        sessionActivity?.SetTag("dustwood.max_turns", maxTurns);

        // Connect to MCP Server
        Console.WriteLine($"Connecting to MCP server at {mcpUrl}...");
        var transport = new HttpClientTransport(new HttpClientTransportOptions
        {
            Endpoint = new Uri(mcpUrl)
        });

        var clientOptions = new McpClientOptions
        {
            ProtocolVersion = "2025-11-25"
        };

        await using var mcpClient = await McpClient.CreateAsync(transport, clientOptions);

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

        string prompt = $"Start game. Issue LOOK to inspect your starting location, " +
            $"then continue playing for up to {maxTurns} turns to increase your score.";
        int turnCount = 0;

        // ChatClientAgent.RunAsync/RunStreamingAsync loop internally on tool calls until the
        // model stops requesting them - a single call can play the whole game. To honor
        // max_turns we stream the run and cancel the token as soon as the game state we observe
        // reaches the cap or the game ends, rather than checking a turn count after the fact.
        using var cts = new CancellationTokenSource();
        var overallStopwatch = Stopwatch.StartNew();
        double lastCheckpointMs = 0;

        // Span hierarchy follows OTel GenAI semantic conventions so Logfire's Gen AI views pick
        // them up: maf.session (invoke_agent) -> chat {model} (one per LLM completion) ->
        // execute_tool {name} (one per tool call the model made in that completion).
        Activity? chatActivity = null;
        Activity? toolActivity = null;
        var assistantText = new StringBuilder();
        var assistantReasoning = new StringBuilder();

        void FlushAssistantContent()
        {
            if (assistantText.Length > 0)
            {
                string t = assistantText.ToString();
                chatActivity?.AddEvent(new ActivityEvent("gen_ai.assistant.message",
                    tags: new ActivityTagsCollection { ["content"] = t.Length > 2000 ? t[..2000] : t }));
                LogKv("assistant_text", new() { ["text"] = t });
                assistantText.Clear();
            }
            if (assistantReasoning.Length > 0)
            {
                string r = assistantReasoning.ToString();
                chatActivity?.AddEvent(new ActivityEvent("gen_ai.assistant.reasoning",
                    tags: new ActivityTagsCollection { ["content"] = r.Length > 2000 ? r[..2000] : r }));
                LogKv("assistant_thinking", new() { ["text"] = r });
                assistantReasoning.Clear();
            }
        }

        try
        {
            await foreach (var update in agent.RunStreamingAsync(prompt, cancellationToken: cts.Token))
            {
                if (update.FinishReason != null)
                {
                    LogKv("finish_reason", new() {
                        ["value"] = update.FinishReason.ToString(),
                        ["content_count"] = update.Contents.Count
                    });
                }

                foreach (var content in update.Contents)
                {
                    if (chatActivity == null)
                    {
                        chatActivity = ActivitySource.StartActivity($"chat {rawModel}", ActivityKind.Client);
                        chatActivity?.SetTag("gen_ai.operation.name", "chat");
                        chatActivity?.SetTag("gen_ai.system", "openrouter");
                        chatActivity?.SetTag("gen_ai.provider.name", "openrouter");
                        chatActivity?.SetTag("gen_ai.request.model", rawModel);
                    }

                    if (content is TextContent text)
                    {
                        assistantText.Append(text.Text);
                    }
                    else if (content is TextReasoningContent reasoning)
                    {
                        assistantReasoning.Append(reasoning.Text);
                    }
                    else if (content is ErrorContent err)
                    {
                        chatActivity?.SetStatus(ActivityStatusCode.Error, err.Message);
                        LogKv("assistant_error", new() {
                            ["message"] = err.Message,
                            ["error_code"] = err.ErrorCode
                        });
                    }
                    else if (content is FunctionCallContent call)
                    {
                        turnCount++;

                        string cmd = "";
                        if (call.Arguments != null && call.Arguments.TryGetValue("command", out var cObj))
                        {
                            cmd = cObj?.ToString() ?? "";
                        }

                        toolActivity?.Dispose();
                        toolActivity = ActivitySource.StartActivity($"execute_tool {call.Name}", ActivityKind.Internal);
                        toolActivity?.SetTag("gen_ai.tool.name", call.Name);
                        toolActivity?.SetTag("gen_ai.tool.call.id", call.CallId);
                        toolActivity?.SetTag("dustwood.command", cmd);
                        toolActivity?.SetTag("dustwood.turn", turnCount);
                        if (call.Arguments != null)
                        {
                            try
                            {
                                toolActivity?.SetTag("gen_ai.tool.call.arguments", JsonSerializer.Serialize(call.Arguments));
                            }
                            catch { }
                        }

                        LogKv("tool_call", new() {
                            ["tool"] = call.Name,
                            ["command"] = cmd
                        });

                        if (delay > 0 && !string.Equals(cmd, "LOOK", StringComparison.OrdinalIgnoreCase))
                        {
                            await Task.Delay(delay * 1000);
                        }

                        if (turnCount >= maxTurns * 3)
                        {
                            stopReason = "safety_limit_reached";
                            cts.Cancel();
                        }
                    }
                    else if (content is FunctionResultContent result)
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

                        if (toolActivity != null)
                        {
                            string resultStr = lastOutput.Length > 500 ? lastOutput[..500] : lastOutput;
                            toolActivity.SetTag("gen_ai.tool.call.result", resultStr);
                        }

                        if (lastState != null)
                        {
                            currentTurn = lastState.Turns;
                            toolActivity?.SetTag("dustwood.game_turn", currentTurn);
                            toolActivity?.SetTag("dustwood.score", lastState.Score);

                            if (!lastState.IsPlaying)
                            {
                                stopReason = "game_over";
                                cts.Cancel();
                            }
                            else if (currentTurn >= maxTurns)
                            {
                                stopReason = "max_turns_reached";
                                cts.Cancel();
                            }
                        }

                        toolActivity?.Dispose();
                        toolActivity = null;
                    }
                    else if (content is UsageContent usage)
                    {
                        double nowMs = overallStopwatch.Elapsed.TotalMilliseconds;
                        double latencyMs = nowMs - lastCheckpointMs;
                        lastCheckpointMs = nowMs;
                        totalLatencyMs += latencyMs;

                        int inputTokens = (int)(usage.Details.InputTokenCount ?? 0);
                        int outputTokens = (int)(usage.Details.OutputTokenCount ?? 0);
                        int callTokens = inputTokens + outputTokens;

                        totalInputTokens += inputTokens;
                        totalOutputTokens += outputTokens;
                        totalTokens += callTokens;

                        chatActivity?.SetTag("gen_ai.usage.input_tokens", inputTokens);
                        chatActivity?.SetTag("gen_ai.usage.output_tokens", outputTokens);
                        chatActivity?.SetTag("dustwood.latency_ms", Math.Round(latencyMs, 2));

                        LogKv("provider_call", new() {
                            ["model"] = rawModel,
                            ["input_tokens"] = inputTokens,
                            ["output_tokens"] = outputTokens,
                            ["total_tokens"] = callTokens,
                            ["latency_ms"] = Math.Round(latencyMs, 2)
                        });

                        // Microsoft.Extensions.AI.OpenAI's response types don't know about
                        // OpenRouter's non-standard delta.reasoning field, so TextReasoningContent
                        // never fires. ReasoningSniffingPolicy pulls it straight off the wire instead.
                        string sniffedReasoning = FlushReasoningSink();
                        if (sniffedReasoning.Length > 0)
                        {
                            assistantReasoning.Append(sniffedReasoning);
                        }

                        FlushAssistantContent();
                        chatActivity?.Dispose();
                        chatActivity = null;
                    }
                }
            }
        }
        catch (OperationCanceledException)
        {
            // Expected: the stream is cancelled once the turn cap or game-over state is observed.
        }
        catch (Exception ex)
        {
            (toolActivity ?? chatActivity)?.SetStatus(ActivityStatusCode.Error, ex.Message);
            if (toolActivity != null) toolActivity.AddException(ex); else chatActivity?.AddException(ex);
            Console.WriteLine($"Agent execution error: {ex.Message}");
            stopReason = $"error: {ex.Message}";
        }
        finally
        {
            // Catches any assistant text/reasoning from a final round that never got a
            // terminating UsageContent (e.g. the stream was cancelled or the model stopped
            // without the provider reporting usage).
            FlushAssistantContent();
            toolActivity?.Dispose();
            chatActivity?.Dispose();
        }

        if (stopReason == "completed" && currentTurn >= maxTurns) stopReason = "max_turns_reached";
        else if (stopReason == "completed" && lastState != null && !lastState.IsPlaying) stopReason = "game_over";

        bool win = lastState != null && lastState.Score >= 100;
        bool loss = lastState != null && (!lastState.IsPlaying && !win);

        sessionActivity?.SetTag("dustwood.turns", currentTurn);
        sessionActivity?.SetTag("dustwood.final_score", lastState?.Score ?? 0);
        sessionActivity?.SetTag("dustwood.stop_reason", stopReason);
        sessionActivity?.SetTag("dustwood.win", win);
        sessionActivity?.SetTag("gen_ai.usage.input_tokens", totalInputTokens);
        sessionActivity?.SetTag("gen_ai.usage.output_tokens", totalOutputTokens);

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
        options.AddPolicy(new ReasoningSniffingPolicy(), PipelinePosition.PerCall);

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
