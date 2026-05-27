# Package Dependencies

Third-party packages used across the Echoes of Dustwood project, organized by purpose.

## Python Dependencies

### Core Infrastructure
| Package | Purpose | Used In |
|---------|---------|---------|
| `pydantic` | Data validation & parsing | All framework packages, shared |
| `python-dotenv` | Environment variable management | All packages |
| `httpx` | Async HTTP client | shared (LLM observability) |

### AI Frameworks
| Package | Purpose | Used In |
|---------|---------|---------|
| `pydantic-ai` | Pydantic AI agent framework | pydantic package |
| `agno` | Agno agent framework | agno package |
| `strands-agents` | Strands Agents framework | strands package |
| `google-adk` | Google AI Development Kit with MCP extensions | adk package |

### LLM Provider SDKs
| Package | Purpose | Used In |
|---------|---------|---------|
| `anthropic` | Anthropic Claude API | agno (framework support) |
| `openai` | OpenAI API | agno (framework support) |
| `google-genai` | Google Generative AI API | agno (framework support) |
| `ollama` | Local Ollama model integration | agno (framework support) |
| `litellm` | LLM abstraction layer (unified API) | strands (model routing) |

### Model Context Protocol (MCP)
| Package | Purpose | Used In |
|---------|---------|---------|
| `mcp` | MCP SDK for structured tool communication | agno, strands |

### Visualization & Analysis
| Package | Purpose | Used In |
|---------|---------|---------|
| `matplotlib` | Plotting and data visualization | charts (benchmark visualizations) |
| `numpy` | Numerical computing | charts (data processing) |

### Internal
| Package | Purpose | Used In |
|---------|---------|---------|
| `vibepascal-shared` | Guidance loader, LLM observability, MCP command policy | All framework packages |

---

## Go Dependencies

### Direct Dependencies
| Package | Purpose |
|---------|---------|
| `github.com/modelcontextprotocol/go-sdk` | MCP server implementation (structured tool definitions, JSON-RPC transport) |
| `golang.org/x/term` | Terminal I/O control (raw mode for game input) |
| `gopkg.in/ini.v1` | INI file parsing (world.ini configuration) |

### Indirect Dependencies
| Package | Purpose |
|---------|---------|
| `github.com/google/jsonschema-go` | JSON schema validation (MCP tool definitions) |
| `github.com/segmentio/asm` | Assembly utilities (encoding optimization) |
| `github.com/segmentio/encoding` | Efficient JSON/binary encoding |
| `github.com/stretchr/testify` | Testing assertions and utilities |
| `github.com/yosida95/uritemplate/v3` | URI template processing |
| `go.etcd.io/bbolt` | Embedded key-value store (if used by MCP SDK) |
| `golang.org/x/oauth2` | OAuth2 support (potential provider integration) |
| `golang.org/x/sys` | System-level calls (signal handling, platform-specific I/O) |

---

## Dependency Rationale

### Why These LLM Providers?
Multiple provider SDKs (Anthropic, OpenAI, Google, Ollama) are included to support the framework comparison. Each framework can be configured to use different providers via environment variables.

### Why Both Pydantic AI and Alternative Frameworks?
The project compares 4 different AI agent frameworks:
1. **Pydantic AI** — Modern, Python-native, Pydantic integration
2. **Agno** — Feature-rich with built-in multi-provider support
3. **Strands** — LiteLLM-based for flexible model routing
4. **Google ADK** — Google-native with integrated MCP tooling

### Why MCP?
MCP (Model Context Protocol) allows frameworks to call game commands via structured tool definitions rather than text parsing, enabling:
- Reliable command execution with JSON responses
- Consistent agent behavior across frameworks
- Extensibility for new game commands

### Why Go for the Game Server?
The Go implementation provides:
- Better concurrency for MCP (HTTP + stdio servers)
- Stronger type safety than Pascal
- Cross-platform binary distribution
- Reliable signal handling and graceful shutdown

---

## Version Strategy

- **Framework packages:** Allow minor/patch updates (`>=` constraints)
- **Pydantic AI:** Pinned to `2.0.0b3` (early access, API stability required)
- **Google ADK:** Pinned to `2.0.0` (breaking changes expected in future)
- **Python:** Requires `>=3.12` across all packages
- **Go:** Go 1.24.0

## Related Documentation

- **Overview Index:** [README.md](file:///home/mfranz/github/vibepascal/README.md)
- **Framework Packages Setup:** [packages/README.md](file:///home/mfranz/github/vibepascal/packages/README.md) — How isolated framework virtual environments are structured.
- **Client Implementation Details:** [packages/IMPL.md](file:///home/mfranz/github/vibepascal/packages/IMPL.md) — Comparative overview of each client's dependencies and logic.
