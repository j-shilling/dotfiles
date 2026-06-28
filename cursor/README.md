# Cursor Configuration

This stow package deploys the **Cursor harness overlay** to `~/.cursor/` and `~/.config/Cursor/`.

## Contents

| Path (in repo) | Deploys to | Purpose |
|----------------|------------|---------|
| `.cursor/permissions.json` | `~/.cursor/permissions.json` | MCP server allowlist |
| `.config/Cursor/User/settings.json` | `~/.config/Cursor/User/settings.json` | Editor preferences |

## Installation

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v cursor
# or: make cursor
```

If `~/.cursor/permissions.json` or `~/.config/Cursor/User/settings.json` already exist as regular files, back them up before the first stow:

```bash
cp ~/.cursor/permissions.json ~/.cursor/permissions.json.pre-stow-backup
cp ~/.config/Cursor/User/settings.json ~/.config/Cursor/User/settings.json.pre-stow-backup
make cursor
```

## Local overrides

Create local overrides for machine-specific settings (never committed):

```bash
cp ~/dotfiles/cursor/.cursor/permissions.json ~/.cursor/permissions.local.json
# Cursor does not natively merge local overrides — manage manually.
```

## What's NOT included

Excluded via `.stow-local-ignore`:

- `projects/`, `extensions/`, `plugins/`, `worktrees/` — auto-generated agent state
- `argv.json` — contains unique `crash-reporter-id`
- `agent-cli-state.json` — auto-generated tracking state
- `skills-cursor/` — Cursor-cloud-managed synced skills
- `ai-tracking/`, `plans/`, `subagents/` — runtime state
- `globalStorage/`, `workspaceStorage/`, `History/`, `CachedData/` — VS Code-derived state

## Project context (dotfiles repo)

- **[AGENTS.md](../AGENTS.md)** — canonical OAF manifest (Cursor reads natively)
- **[docs/agents/harness-mapping.md](../docs/agents/harness-mapping.md)** — cross-harness mapping

## See also

- [Harness mapping](../docs/agents/harness-mapping.md)
- [Portability and overrides](../docs/agents/portability-and-overrides.md)