# Agents Stow Package

This stow package deploys user-level AI harness configuration for Claude Code,
Codex, Copilot CLI, Cursor, OpenCode, and the user-level OAF directory.

## What this package contains

| Path (in repo) | Deploys to | Purpose |
|----------------|------------|---------|
| `.claude/` | `~/.claude/` | Claude Code user settings and bridge instructions |
| `.codex/AGENTS.md` | `~/.codex/AGENTS.md` | Global Codex guidance; runtime config stays local |
| `.copilot/` | `~/.copilot/` | Copilot CLI settings and MCP config |
| `.cursor/permissions.json` | `~/.cursor/permissions.json` | Cursor permission allowlist |
| `.config/Cursor/User/settings.json` | `~/.config/Cursor/User/settings.json` | Cursor editor settings |
| `.config/agents/` | `~/.config/agents/` | User-level OAF manifest, meta-skills, MCP, and sub-agent links |
| `.config/opencode/opencode.jsonc` | `~/.config/opencode/opencode.jsonc` | OpenCode provider, MCP, agent tool policies |

The package intentionally does not track `~/.codex/config.toml`, Codex profile
files, auth, sessions, plugin cache, or other machine-local Codex state.

## Installation

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v agents
# or: make agents
```

## Project-Level OAF Definitions

Project-level cross-tool agent configuration lives outside this package:

| Path | Purpose |
|------|---------|
| [AGENTS.md](../AGENTS.md) | OAF v0.8.0 canonical manifest |
| [CLAUDE.md](../CLAUDE.md) | `@AGENTS.md` bridge for Claude Code |
| [skills/](../skills/) | Local AgentSkills.io skills |
| [mcp-configs/](../mcp-configs/) | OAF MCP server configs |
| [subagents/](../subagents/) | OAF sub-agent definitions |
| [docs/agents/](../docs/agents/) | Reference documentation |

OpenCode reads `AGENTS.md` natively and uses `opencode.jsonc` as a harness
overlay. Codex reads the repo root `AGENTS.md` for project context and
`~/.codex/AGENTS.md` for global guidance.

## See Also

- [Harness mapping](../docs/agents/harness-mapping.md)
- [Open Agent Format](https://openagentformat.com/)
- [OpenCode config schema](https://opencode.ai/config.json)
