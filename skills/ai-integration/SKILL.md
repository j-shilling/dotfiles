---
name: "ai-integration"
description: >
  Configure GPTel, MCP servers, and AI tools in Emacs. Use when editing
  init-ai.el, init-ai-tools.el, gptel backends, mcp-hub-servers, prompts/
  templates, or adding new agent tool definitions — even if the user only says
  "AI", "Claude", "MCP", or "gptel".
license: "MIT"
metadata:
  author: "j-shilling"
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# AI Integration (Emacs)

## Key files

- [init-ai.el](emacs/.config/emacs/init-ai.el) — GPTel backends, MCP hub servers
- [init-ai-tools.el](emacs/.config/emacs/init-ai-tools.el) — GPTel tool definitions
- [prompts/](emacs/.config/emacs/prompts/) — Reusable AI prompt templates
- [lisp/gptel-*](emacs/.config/emacs/lisp/) — Custom GPTel extensions

## GPTel backends (current)

| Backend | Models | Key source |
|---------|--------|------------|
| Anthropic Claude | `claude-sonnet-4-5-20250929`, `claude-opus-4-6` | `password-store` (`anthropic-api-key`) |
| GitHub Copilot | `gpt-5.2`, `claude-sonnet-4.6`, `gpt-5.2-codex` | Copilot auth |
| Ollama | `qwen3:8b`, `deepseek-coder-v2:16b` | Local |

Default backend: Copilot (`gpt-5.2`). Temperature: 0.0. Tools: buffers + filesystem.

## Adding an MCP server

Add an entry to `mcp-hub-servers` in `init-ai.el`:

```elisp
("server-name" . (:command "npx" :args ("-y" "package-name")))
```

Or use `:url` for remote SSE endpoints. Requires `mcp` and `mcp-hub` packages.

Known quirk: `notion` appears twice in `mcp-hub-servers` — consolidate only if explicitly asked.

## Adding a GPTel tool

Define tools in `init-ai-tools.el` using `gptel-make-tool` or require modules from `lisp/`. Follow existing patterns in `gptel-buffers` and `gptel-filesystem`.

## Adding a prompt template

Create a new `.md` file in `prompts/` following existing templates (dev-work-planner, effect-backend, etc.).

## API keys

Never commit API keys. Use `password-store` (`init-get-anthropic-key` in init-ai.el).

## Deep reference

See [docs/agents/ai-integration.md](docs/agents/ai-integration.md) for the full MCP server table and cross-harness notes.
