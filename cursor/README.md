# Cursor Configuration

This stow package deploys the **Cursor harness overlay** to `~/.cursor/` and `~/.config/Cursor/`.

## Contents

| Path (in repo) | Deploys to | Purpose |
|----------------|------------|---------|
| `.cursor/permissions.json` | `~/.cursor/permissions.json` | MCP server allowlist |
| `.config/Cursor/User/settings.json` | `~/.config/Cursor/User/settings.json` | Editor preferences + default model |

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

## OpenRouter Integration

Cursor is routed through OpenRouter using the **Override OpenAI Base URL** feature. The default model is `openrouter/pareto-code`.

### One-time setup (manual — cannot be stowed)

In Cursor: **Settings → Models → API Keys**:

| Field | Value |
|-------|-------|
| Toggle on | OpenAI API Key |
| API Key | `sk-or-...` (your OpenRouter key) |
| Toggle on | Override OpenAI Base URL |
| Base URL | `https://openrouter.ai/api/v1/cursor` |

Then in **Models**, click **+ Add model** and add:

```
openrouter/pareto-code
```

### API key storage

Store your OpenRouter key in `pass`:

```bash
pass insert openrouter/api-key
```

The key is never committed to this repo. Cursor stores it internally in the OS keychain.

### Pareto Code Router

The [Pareto Router](https://openrouter.ai/openrouter/pareto-code) maintains a tiered shortlist of coding models ranked by Artificial Analysis coding percentiles. It defaults to the **High** tier — the strongest coding models.

Tune the quality/cost tradeoff via `min_coding_score` (0–1) at [OpenRouter Plugin Settings](https://openrouter.ai/settings/plugins) under **Pareto Router**:

| `min_coding_score` | Tier | Models |
|--------------------|------|--------|
| ≥ 0.55 | High (default) | Strongest coders |
| ≥ 0.15 | Medium | Balanced coders |
| 0 | Low | Fastest coders |

For lower-latency responses, use the Nitro variant (`openrouter/pareto-code:nitro`) which sorts the tiered models by measured throughput.

### Subagent model

The `settings.json` in this package sets `cursor.composer.subagentModel` to `openrouter/pareto-code` so that Cursor subagents also route through OpenRouter.

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

### Not stowable (UI-only)

Model selection, API keys, and base URL overrides are managed through Cursor's internal keychain/crypto storage and cannot be configured via `settings.json`.

## Project context (dotfiles repo)

- **[AGENTS.md](../AGENTS.md)** — canonical OAF manifest (Cursor reads natively)
- **[docs/agents/harness-mapping.md](../docs/agents/harness-mapping.md)** — cross-harness mapping

## See also

- [Harness mapping](../docs/agents/harness-mapping.md)
- [Portability and overrides](../docs/agents/portability-and-overrides.md)
- [OpenRouter Cursor integration](https://openrouter.ai/docs/cookbook/coding-agents/cursor-integration)
- [Pareto Router docs](https://openrouter.ai/docs/guides/routing/routers/pareto-router)