"""In-process regression checks for the GPT-OSS tool-name hardening in
`pydantic_mcp_client.py`. No pytest dependency, no live MCP server, no API key, no network
access - run directly:

    cd packages/pydantic && uv run python tests/test_hardening.py

Exercises the `after_model_request` Harmony tool-name repair against `FunctionModel`-based
fake models.
"""

import asyncio
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from pydantic_ai import Agent
from pydantic_ai.messages import ModelResponse, TextPart, ToolCallPart
from pydantic_ai.models.function import AgentInfo, FunctionModel

from pydantic_mcp_client import HARMONY_TOOL_NAME_SUFFIX, _tool_name_hooks


async def test_harmony_name_repair_executes_the_tool() -> None:
    calls = []

    def respond(messages: list, info: AgentInfo) -> ModelResponse:
        if not calls:
            return ModelResponse(parts=[ToolCallPart(tool_name=f"command{HARMONY_TOOL_NAME_SUFFIX}", args={})])
        return ModelResponse(parts=[TextPart("done")])

    agent = Agent(model=FunctionModel(respond), capabilities=[_tool_name_hooks], retries=1)

    @agent.tool_plain
    def command(**kwargs) -> str:
        calls.append(kwargs)
        return "ok"

    await agent.run("play")
    assert calls, "malformed Harmony tool name was never repaired/executed"
    print("PASS: harmony name repair executes the tool")


async def test_valid_tool_name_passes_through_unchanged() -> None:
    calls = []

    def respond(messages: list, info: AgentInfo) -> ModelResponse:
        if not calls:
            return ModelResponse(parts=[ToolCallPart(tool_name="command", args={})])
        return ModelResponse(parts=[TextPart("done")])

    agent = Agent(model=FunctionModel(respond), capabilities=[_tool_name_hooks], retries=1)

    @agent.tool_plain
    def command(**kwargs) -> str:
        calls.append(kwargs)
        return "ok"

    await agent.run("play")
    assert calls, "a valid tool name should still execute normally"
    print("PASS: valid tool name unaffected")


async def main() -> None:
    await test_harmony_name_repair_executes_the_tool()
    await test_valid_tool_name_passes_through_unchanged()
    print("\nAll hardening checks passed.")


if __name__ == "__main__":
    asyncio.run(main())
