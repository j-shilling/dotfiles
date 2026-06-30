# Harness Mapping

How AI coding agents read configuration in this repository and the user-level OAF directory.

## Canonical sources

Two OAF manifests work together:

| Level | File | Purpose |
|-------|------|---------|
| **Project** | `AGENTS.md` (repo root) | Project context, skills, MCP references, sub-agent composition for dotfiles work |
| **User** | `~/.config/agents/AGENTS.md` | User-level OAF meta-skills, shared MCP configs, OAF sub-agent references |

## Tool-by-tool mapping

| Tool | Reads | Bridge / overlay (from `agents/` package) |
|------|-------|-------------------------------------------|
| **Cursor** | `AGENTS.md` natively | `~/.config/Cursor/User/settings.json` + `~/.cursor/permissions.json` |
| **Codex CLI** | `AGENTS.md` natively | `~/.codex/config.toml` + local `config.local.toml` |
| **OpenCode** | `AGENTS.md` + falls back to `CLAUDE.md` | `~/.config/opencode/opencode.jsonc` (points at `~/.config/agents/skills/`) |
| **Claude Code** | `CLAUDE.md` → `@AGENTS.md` import | User prefs: `~/.claude/CLAUDE.md` + `~/.claude/settings.json` |
| **Copilot CLI** | Project context varies | `~/.copilot/settings.json` + `mcp-config.json` |
| **Emacs gptel** | Own config | `init-ai.el` — separate harness |

## File roles

```
AGENTS.md              ← OAF manifest (project-level, canonical)
CLAUDE.md              ← @AGENTS.md (Claude Code bridge)
~/.config/agents/      ← OAF user-level directory (shared skills, subagents, MCP)
skills/                ← AgentSkills.io local skills
mcp-configs/           ← OAF MCP server configs
subagents/             ← OAF sub-agent definitions
docs/agents/           ← Progressive disclosure reference
```

## User-level OAF directory (`~/.config/agents/`)

The `~/.config/agents/` directory is the canonical user-level OAF home, deployed from the `agents/` package:

```
~/.config/agents/
├── AGENTS.md              ← OAF manifest for user-level agents, skills, MCP
├── README.md
├── skills/                ← User-level skills (harness-agnostic)
│   ├── create-oaf-agent/  ← Meta-skill for scaffolding OAF agents
│   └── create-agent-skill/ ← Meta-skill for AgentSkills.io authoring
├── subagents/             ← Symlinks to repo root subagents/
└── mcp-configs/           ← Symlinks to repo root mcp-configs/
```

### How each harness discovers it

- **OpenCode**: `opencode.jsonc` references `~/.config/agents/skills/` in its `skills.dirs` array
- **Claude Code**: `~/.claude/CLAUDE.md` references `~/.config/agents/AGENTS.md` alongside `@AGENTS.md`
- **Codex CLI**: Reads root `AGENTS.md` natively; Codex-specific config in `~/.codex/config.toml`
- **Cursor**: Reads root `AGENTS.md` natively; can also discover `~/.config/agents/` through skill discovery
- **Copilot CLI**: Does not support OAF; uses native `~/.copilot/settings.json` and `mcp-config.json`

## Claude Code layers

Precedence (highest to lowest):

1. Managed/enterprise settings
2. Command-line arguments
3. Project `.claude/settings.local.json`
4. Project `.claude/settings.json`
5. User `~/.claude/settings.json` (stowed from `agents/` package)
6. Project `CLAUDE.md` → imports `AGENTS.md`
7. User `~/.claude/CLAUDE.md` → references `~/.config/agents/AGENTS.md`
8. `~/.config/agents/AGENTS.md` — user-level OAF (skills, subagents, MCP)

## OpenCode harness overlay

Config at `~/.config/opencode/opencode.jsonc`:

- Provider, MCP, per-agent tool policies
- Skills discovery via `~/.config/agents/skills/` and `.opencode/skills/`
- OpenCode also discovers `~/.config/agents/agents/` for user-level agent definitions

## Codex CLI harness overlay

Config at `~/.codex/config.toml`:

- Model provider, MCP servers, plugins
- `config.local.toml` for secrets and project trust (gitignored)
- Reads root `AGENTS.md` natively for project context

## Copilot CLI

Config at `~/.copilot/`:

- `settings.json` — model, plugins, marketplaces
- `mcp-config.json` — MCP servers (terraform, aws, notion, playwright)
- No OAF adaptor yet; uses native config format

## Cursor

Config at `~/.config/Cursor/User/settings.json` and `~/.cursor/permissions.json`:

- Editor preferences (window appearance, UI)
- MCP server allowlist via `permissions.json`
- Reads `AGENTS.md` natively from project root
- Supports `.cursor/rules/*.mdc` for AI behavior rules

Project-level `.cursor/rules/`, `.cursor/mcp.json`, and `.cursor/agents/` stay in each project repository (not stowed globally).

## Emacs gptel

Not OAF-converted. Configuration lives entirely in Emacs Lisp:

- `init-ai.el` — backends, MCP hub
- `init-ai-tools.el` — tool definitions
- `prompts/` — templates

Changes to Emacs AI config do not automatically propagate to other harnesses. Update `docs/agents/ai-integration.md` when MCP servers or models change.

## OAF composition in this repo

Root `AGENTS.md` frontmatter references:

| Type | Location |
|------|----------|
| Skills | `skills/stow-dotfiles/`, `skills/emacs-config/`, `skills/ai-integration/` |
| MCP configs | `mcp-configs/playwright/`, `context7/`, `filesystem/` |
| Sub-agents | `subagents/emacs-maintainer/`, `subagents/config-reviewer/` |

User-level `~/.config/agents/AGENTS.md` frontmatter references:

| Type | Location |
|------|----------|
| Skills (meta) | `~/.config/agents/skills/create-oaf-agent/`, `create-agent-skill/` |
| MCP configs | `~/.config/agents/mcp-configs/` (symlinks to repo root) |
| Sub-agents | `~/.config/agents/subagents/` (symlinks to repo root) |

## Adding a new harness

1. Check if the tool reads `AGENTS.md` natively
2. If not, create a thin bridge file (like `CLAUDE.md` with `@AGENTS.md`)
3. Put harness-specific config (MCP creds, tool policies) in the unified `agents/` stow package or a `.gitignore`d local file
4. Document the mapping in this file

## Standards reference

- [Open Agent Format](https://openagentformat.com/) — OAF v0.8.0 manifest spec
- [AGENTS.md](https://agents.md/) — OpenAI/Linux Foundation agent instruction convention (OAF builds on this)
- [AgentSkills.io](https://agentskills.io/) — SKILL.md format for modular capabilities