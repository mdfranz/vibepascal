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
    Write-Host "Echoes of Dustwood: Multi-Client MCP PowerShell Runner" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Note: This script requires the Go MCP server to be running."
    Write-Host "      You can start it with: .\bin\dustwood-go.exe --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response"
    Write-Host ""
    Write-Host "Usage: .\play-mcp-game.ps1 <model> <max_turns> [delay] [difficulty] [-Summarize] [-Windowing] [-WindowSize SIZE] [-SessionId ID]"
    Write-Host ""
    Write-Host "Examples:"
    Write-Host "  powershell -ExecutionPolicy Bypass -File .\play-mcp-game.ps1 google/gemini-2.5-flash 15"
    Write-Host "  powershell -ExecutionPolicy Bypass -File .\play-mcp-game.ps1 google/gemini-2.5-flash 25 1 full -Windowing"
    Exit 1
}

if ($Help -or -not $Model -or $MaxTurns -le 0) {
    Show-Usage
}

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "STARTING MULTI-CLIENT MCP SESSION (PowerShell)" -ForegroundColor Cyan
Write-Host "Model: $Model, Level: $Level, Delay: ${Delay}s, Max Turns: $MaxTurns" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan

# Run MAF Agent
Write-Host ""
Write-Host "--- Running Microsoft Agent Framework (.NET OpenRouter) ---" -ForegroundColor Green
powershell -ExecutionPolicy Bypass -File ".\maf-mcp-game.ps1" $Model $MaxTurns $Delay $Level

Write-Host ""
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "ALL MCP CLIENT SESSIONS COMPLETE" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan
