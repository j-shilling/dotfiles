# Codex CLI Configuration

This stow package deploys the **Codex CLI harness overlay** to `~/.codex/`.

## Contents

| Path (in repo) | Deploys to | Purpose |
|----------------|------------|---------|
| `.codex/config.toml` | `~/.codex/config.toml` | Shared plugins, MCP, Ollama profile |
| `.codex/config.local.toml.example` | (documentation) | Template for secrets and project trust |
| `.codex/skills/*` | `~/.codex/skills/*` | Symlinks to root OAF `skills/` |

## Installation

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v codex
# or: make codex
```

If `~/.codex/config.toml` already exists as a regular file, back it up and remove it before the first stow:

```bash
cp ~/.codex/config.toml ~/.codex/config.toml.pre-stow-backup
mv ~/.codex/config.toml ~/.codex/config.toml.bak
make codex
# Merge any needed sections from the backup into config.local.toml
```

## Local overrides

Copy and edit secrets locally (never committed):

```bash
cp ~/dotfiles/codex/.codex/config.local.toml.example ~/.codex/config.local.toml
```

Codex merges `config.toml` with `config.local.toml`. Use local config for:

- Grafana or other MCP credentials
- `[projects."..."]` trust levels
- Work-project MCP blocks (e.g. CoachBridge `.codex/mcp/`)

## Skills

Root OAF skills in `skills/` are symlinked into `~/.codex/skills/` for discovery. Project-specific skills stay in each repo's `.codex/skills/`.

## What's NOT included

Excluded via `.stow-local-ignore`:

- `auth.json`, sessions, cache, plugins cache, SQLite state
- `config.local.toml` (machine secrets)

## Project context (dotfiles repo)

- **[AGENTS.md](../AGENTS.md)** — canonical OAF manifest (Codex reads natively)
- **[docs/agents/harness-mapping.md](../docs/agents/harness-mapping.md)** — cross-harness mapping

## Security note

If a Grafana service account token was ever stored in a committed config file, rotate it in Grafana Cloud and keep the new value only in `config.local.toml`.

## See also

- [Harness mapping](../docs/agents/harness-mapping.md)
- [Portability and overrides](../docs/agents/portability-and-overrides.md)
