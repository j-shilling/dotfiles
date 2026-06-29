# Harness Mapping

How AI coding agents read configuration in this repository and the user-level OAF directory.

## Canonical sources

Two OAF manifests work together:

| Level | File | Purpose |
|-------|------|---------|
| **Project** | `AGENTS.md` (repo root) | Project context, skills, MCP references, sub-agent composition for dotfiles work |
| **User** | `~/.config/agents/AGENTS.md` | User-level OAF meta-skills, shared MCP configs, OAF sub-agent references |

## Tool-by-tool mapping

| Tool | Reads | Bridge / overlay |
|------|-------|------------------|
| **Cursor** | `AGENTS.md` natively | [Cursor settings](../../agents/.config/Cursor/User/settings.json) + [permissions](../../agents/.cursor/permissions.json) |
| **Codex CLI** | `AGENTS.md` natively | [global guidance](../../agents/.codex/AGENTS.md); live `~/.codex/config.toml` stays local |
| **OpenCode** | `AGENTS.md` + falls back to `CLAUDE.md` | [agents/.config/opencode/opencode.jsonc](../../agents/.config/opencode/opencode.jsonc) |
| **Claude Code** | `CLAUDE.md` -> `@AGENTS.md` import | User prefs: [agents/.claude/CLAUDE.md](../../agents/.claude/CLAUDE.md) + [settings](../../agents/.claude/settings.json) |
| **Copilot CLI** | Project context varies | [agents/.copilot/](../../agents/.copilot/) settings + MCP |
| **Emacs gptel** | Own config | [init-ai.el](../../emacs/.config/emacs/init-ai.el) - separate harness |

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

User-level portable Codex assets belong under `~/.agents` when present:
`~/.agents/skills`, `~/.agents/plugins/marketplace.json`, and any cross-harness
agent source material in `~/.agents/agents` or `~/.agents/subagents`.

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
- **Codex CLI**: Reads root `AGENTS.md` natively, reads `~/.codex/AGENTS.md` for global guidance, and keeps runtime config in local `~/.codex/config.toml`
- **Cursor**: Reads root `AGENTS.md` natively; can also discover `~/.config/agents/` through skill discovery
- **Copilot CLI**: Does not support OAF; uses native `~/.copilot/settings.json` and `mcp-config.json`

User-level meta-skills live under `~/.config/agents/skills/`. Root repo skills
remain under `skills/` for project-level dotfiles work.

## Codex global guidance

The unified `agents/` stow package deploys one file to `~/.codex/`:

- `AGENTS.md` - global Codex guidance that points Codex at `~/.agents` for
  portable user-level skills, plugin marketplace metadata, and agent source
  material when present.

Codex reads `AGENTS.md` natively. The live `~/.codex/config.toml`, profile
files such as `~/.codex/work.config.toml`, auth, sessions, plugin cache, SQLite
state, memories, history, and logs are machine-local and excluded from Stow.
Do not use Stow `--adopt` on Codex config or state.

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

## Codex CLI

Global guidance at `~/.codex/AGENTS.md`:

- Points Codex at portable user-level assets under `~/.agents`
- Reads root `AGENTS.md` natively for project context
- Leaves model providers, MCP servers, plugins, profiles, secrets, and project trust in local `~/.codex/config.toml` and related profile files

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
