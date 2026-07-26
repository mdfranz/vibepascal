[CmdletBinding()]
param(
    [Parameter(Position=0, Mandatory=$false)]
    [string]$Model,

    [Parameter(Position=1, Mandatory=$false)]
    [int]$MaxTurns = 0,

    [Parameter(Position=2, Mandatory=$false)]
    [int]$Delay = 1,

    [Parameter(Position=3, Mandatory=$false)]
    [string]$Level = "full",

    [switch]$Summarize,

    [Alias("w")]
    [switch]$Windowing,

    [Alias("n")]
    [int]$WindowSize = 6,

    [string]$SessionId = "",
    [switch]$Help
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
if ($ScriptDir) { Set-Location $ScriptDir }

function Show-Usage {
    Write-Host "Echoes of Dustwood: Microsoft Agent Framework (.NET) MCP PowerShell Runner" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Note: This script requires the Go MCP server to be running."
    Write-Host "      You can start it with: .\bin\dustwood-go.exe --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response"
    Write-Host ""
    Write-Host "Usage: .\maf-mcp-game.ps1 <model> <max_turns> [delay] [difficulty] [-Summarize] [-Windowing] [-WindowSize SIZE] [-SessionId ID]"
    Write-Host ""
    Write-Host "Arguments:"
    Write-Host "  Model             OpenRouter model identifier (e.g. google/gemini-2.5-flash, openai/gpt-4o-mini) (required)"
    Write-Host "  MaxTurns          Maximum turns before stopping (required)"
    Write-Host "  Delay             Seconds to wait between turns (default: 1)"
    Write-Host "  Level             full, medium, minimal (default: full)"
    Write-Host "  -Summarize        Enable history summarization"
    Write-Host "  -Windowing, -w    Enable sliding window history (disabled by default)"
    Write-Host "  -WindowSize, -n   Window size in game turns (default: 6)"
    Write-Host "  -SessionId ID     Resume or create a named session"
    Write-Host ""
    Write-Host "Examples:"
    Write-Host "  .\maf-mcp-game.ps1 google/gemini-2.5-flash 25 1 full"
    Write-Host "  powershell -ExecutionPolicy Bypass -File .\maf-mcp-game.ps1 google/gemini-2.5-flash 25 1 full -Windowing"
    Exit 1
}

if ($Help -or -not $Model -or $MaxTurns -le 0) {
    Show-Usage
}

# Ensure directories exist
New-Item -ItemType Directory -Force -Path "logs" | Out-Null
New-Item -ItemType Directory -Force -Path "data" | Out-Null
New-Item -ItemType Directory -Force -Path "sessions\maf_sessions" | Out-Null

$extraArgs = @()
if ($Summarize) { $extraArgs += "--summarize" }
if ($Windowing) { $extraArgs += "--windowing" }
if ($WindowSize -gt 0) { $extraArgs += "--window-size"; $extraArgs += "$WindowSize" }
if (-not [string]::IsNullOrWhiteSpace($SessionId)) { $extraArgs += "--session-id"; $extraArgs += "$SessionId" }

Write-Host "--- Starting Microsoft Agent Framework (.NET) MCP Agent (Level: $Level, Model: $Model, Delay: ${Delay}s, Max Turns: $MaxTurns) ---" -ForegroundColor Green
Write-Host "--- Ensure MCP Server is running at http://127.0.0.1:8765/mcp ---" -ForegroundColor Yellow

$dotnetArgs = @("run", "--project", "packages/maf/MafMcpClient.csproj", "--", "$Level", "$Model", "$Delay", "$MaxTurns") + $extraArgs
& dotnet @dotnetArgs

Write-Host "--- Session Complete ---" -ForegroundColor Green
