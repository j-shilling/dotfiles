# Codex Global Guidance

The `agents` stow package deploys one global Codex instruction file:

| Path (in repo) | Deploys to | Purpose |
|----------------|------------|---------|
| `agents/.codex/AGENTS.md` | `~/.codex/AGENTS.md` | Global Codex guidance and `~/.agents` routing |

The package intentionally does **not** track Codex runtime configuration.
`~/.codex/config.toml` is machine-local. On this work laptop it can contain
work-account settings, Codex desktop/app state, trusted project paths, plugin
marketplace state, feature flags, hook trust, and other settings that should not
be copied into this shared dotfiles repo.

Use `$HOME/.agents` for portable user-level agent assets when present:

- `$HOME/.agents/skills/*/SKILL.md` - user-level reusable skills
- `$HOME/.agents/plugins/marketplace.json` - personal plugin marketplace
- `$HOME/.agents/agents` or `$HOME/.agents/subagents` - cross-harness agent
  source material, adapted to Codex-supported custom agents when needed

## Installation

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v agents
# or: make agents
```

Run a dry-run first when changing this package:

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -n -R agents
```

Do not use `--adopt` for this package on the work laptop. The existing
`~/.codex/config.toml` should remain a regular local file managed by Codex and
manual local edits, not a Stow symlink.

Keep local `~/.codex` state for:

- Work OpenAI / ChatGPT account model settings
- Codex desktop/app settings
- Installed plugin and marketplace state
- Grafana or other MCP credentials
- `[projects."..."]` trust levels
- Work-project MCP blocks

## What's NOT included

Excluded via `agents/.stow-local-ignore`:

- `config.toml` (machine-local live Codex settings)
- `*.config.toml` profile files and `config.local.toml` (machine secrets)
- `auth.json`, sessions, cache, plugins cache, SQLite state
- `skills/` (use `$HOME/.agents/skills` instead)

## Project Context

- **[AGENTS.md](../../AGENTS.md)** - canonical OAF manifest (Codex reads natively)
- **[docs/agents/harness-mapping.md](../../docs/agents/harness-mapping.md)** - cross-harness mapping

## See Also

- [Harness mapping](../../docs/agents/harness-mapping.md)
- [Portability and overrides](../../docs/agents/portability-and-overrides.md)
