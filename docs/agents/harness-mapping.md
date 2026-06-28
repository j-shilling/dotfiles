# Harness Mapping

How AI coding agents read configuration in this repository.

## Canonical source

**Root [AGENTS.md](../../AGENTS.md)** is the OAF v0.8.0 manifest — single source of truth for project context, skills, MCP references, and sub-agent composition.

## Tool-by-tool mapping

| Tool | Reads | Bridge / overlay |
|------|-------|------------------|
| **Cursor** | `AGENTS.md` natively | None needed |
| **Codex CLI** | `AGENTS.md` natively | [codex/.codex/](../../codex/.codex/) `config.toml` + local `config.local.toml` |
| **OpenCode** | `AGENTS.md` + falls back to `CLAUDE.md` | [agents/.config/opencode/opencode.jsonc](../../agents/.config/opencode/opencode.jsonc) |
| **Claude Code** | `CLAUDE.md` → `@AGENTS.md` import | User prefs: [claude/.claude/CLAUDE.md](../../claude/.claude/CLAUDE.md) (stowed) |
| **Copilot CLI** | Project context varies | [copilot/.copilot/](../../copilot/.copilot/) settings + MCP |
| **Emacs gptel** | Own config | [init-ai.el](../../emacs/.config/emacs/init-ai.el) — separate harness |

## File roles

```
AGENTS.md          ← OAF manifest (canonical)
CLAUDE.md          ← @AGENTS.md (Claude Code bridge)
skills/            ← AgentSkills.io local skills
mcp-configs/       ← OAF MCP server configs
subagents/         ← OAF sub-agent definitions
docs/agents/       ← Progressive disclosure reference
```

## OpenCode harness overlay

The `agents/` stow package deploys to `~/.config/opencode/`:

- `opencode.jsonc` — provider, MCP, per-agent tool policies
- `skills/create-oaf-agent/` — meta-skill for scaffolding OAF agents
- `skills/create-agent-skill/` — meta-skill for AgentSkills.io authoring

These meta-skills are harness-specific tooling, not duplicated in root `skills/`.

Root OAF skills (`skills/`) are symlinked into `~/.config/opencode/skills/` for discovery alongside the meta-skills.

## Codex CLI harness overlay

The `codex/` stow package deploys to `~/.codex/`:

- `config.toml` — plugins, shared MCP, Ollama profile
- `config.local.toml` — secrets and project trust (gitignored; see `config.local.toml.example`)
- `skills/` — symlinks to root `skills/`

Codex reads `AGENTS.md` natively. Ephemeral state (auth, sessions, plugin cache) is excluded via `.stow-local-ignore`.

## Claude Code layers

Precedence (highest to lowest):

1. Managed/enterprise settings
2. Command-line arguments
3. Project `.claude/settings.local.json`
4. Project `.claude/settings.json`
5. User `~/.claude/settings.json` (stowed from `claude/` package)
6. Project `CLAUDE.md` → imports `AGENTS.md`
7. User `~/.claude/CLAUDE.md` (personal preferences)

## Copilot CLI

Stowed from `copilot/` package:

- `settings.json` — model, plugins
- `mcp-config.json` — MCP servers (terraform, aws, notion, playwright)

Ephemeral state (OAuth, sessions) excluded from git.

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

## Adding a new harness

1. Check if the tool reads `AGENTS.md` natively
2. If not, create a thin bridge file (like `CLAUDE.md` with `@AGENTS.md`)
3. Put harness-specific config (MCP creds, tool policies) in a stow package or `.gitignore`d local file
4. Document the mapping in this file

## Standards reference

- [Open Agent Format](https://openagentformat.com/) — OAF v0.8.0 manifest spec
- [AGENTS.md](https://agents.md/) — OpenAI/Linux Foundation agent instruction convention (OAF builds on this)
- [AgentSkills.io](https://agentskills.io/) — SKILL.md format for modular capabilities
