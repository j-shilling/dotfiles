# AI Integration

Emacs AI configuration via GPTel, MCP, and custom tools.

## GPTel configuration

File: `emacs/.config/emacs/init-ai.el`

### Backends

| Backend | Const | Default model | Auth |
|---------|-------|---------------|------|
| Anthropic Claude | `init-ai--claude` | `claude-sonnet-4-5-20250929` | `password-store` (`anthropic-api-key`) |
| GitHub Copilot | `init-ai--copilot` | `gpt-5.2` | Copilot auth |
| Ollama | `init-ai--ollama` | `qwen3:8b` | Local |

**Current defaults**: backend = Copilot, model = `gpt-5.2`, temperature = 0.0, caching enabled, tools enabled.

Claude models available: `claude-sonnet-4-5-20250929`, `claude-opus-4-6`.

Copilot models: `gpt-5.2`, `claude-sonnet-4.6`, `gpt-5.2-codex`.

### Built-in tools

Registered via `gptel-tools`:

- `buffers` — buffer read/write operations
- `filesystem` — file read/write/list/search/replace

Custom tools defined in `init-ai-tools.el` and `lisp/gptel-*`.

### ai-code package

Optional `ai-code` package (if installed):

- Backend: Codex
- Keybinding: `C-c a`
- Magit integration via transients

## MCP servers (Emacs mcp-hub)

Configured in `mcp-hub-servers` alist in `init-ai.el`:

| Server | Connection |
|--------|------------|
| playwright | `npx -y @playwright/mcp@latest` |
| mermaid | `npx -y @peng-shawn/mermaid-mcp-server` |
| a11y | `npx -y a11y-mcp` |
| notion | `npx -y mcp-remote https://mcp.notion.com/mcp` |
| context7 | `npx -y @upstash/context7-mcp` |
| zod | `https://mcp.inkeep.com/zod/mcp` |
| notion (duplicate) | `mcp-remote https://mcp.notion.com/sse` |
| nuxt | `https://nuxt.com/mcp` |
| fetch | `uvx mcp-server-fetch` |
| aws | `uvx mcp-proxy-for-aws@latest` |
| vitest | `npx -y @madrus/vitest-mcp-server@latest` |
| terraform | `docker run hashicorp/terraform-mcp-server:0.4.0` |
| filesystem | `npx -y @modelcontextprotocol/server-filesystem $HOME` |

Note: `notion` is registered twice with different transports. Consolidate only if explicitly requested.

## OAF MCP configs (repo root)

Subset configs for cross-harness use in `mcp-configs/`:

- `playwright/` — browser automation
- `context7/` — library documentation lookup
- `filesystem/` — `$HOME` file operations

Each has `ActiveMCP.json` (tool subset) and `config.yaml` (connection details).

## Custom GPTel tools

File: `init-ai-tools.el`

- `gptel-buffers` — buffer management
- `gptel-filesystem` — filesystem operations
- `create_patch_buffer` — diff display for user review

## Prompt templates

`emacs/.config/emacs/prompts/`:

| Template | Purpose |
|----------|---------|
| `dev-work-planner.md` | Development task planning |
| `effect-backend.md` | Effect.js backend assistant |
| `gptel-tool-definition-assistant.md` | GPTel tool authoring |
| `mermaid-diagram-assistant.md` | Mermaid diagram generation |
| `prompt-generator.md` | Prompt template creation |

## API key management

- Anthropic: `password-store` entry `anthropic-api-key`
- Never commit API keys to the repository
- MCP auth uses environment variables or harness-specific credential stores

## Adding new integrations

1. **MCP server**: add to `mcp-hub-servers` in `init-ai.el`
2. **GPTel tool**: define in `init-ai-tools.el` or new `lisp/gptel-*.el`
3. **Prompt**: add `.md` file to `prompts/`
4. **OAF MCP config**: add `mcp-configs/<name>/` with ActiveMCP.json + config.yaml, reference from root `AGENTS.md`

## Cross-harness notes

| Harness | AI config location |
|---------|-------------------|
| Emacs gptel | `init-ai.el` (this document) |
| Claude Code | User plugins in `claude/.claude/settings.json` |
| OpenCode | `agents/.config/opencode/opencode.jsonc` |
| Copilot CLI | `copilot/.copilot/mcp-config.json` |
| Cursor | Reads root `AGENTS.md`; MCP via Cursor settings |

See [harness-mapping.md](harness-mapping.md) for how each tool reads the OAF manifest.
