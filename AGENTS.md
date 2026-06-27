---
name: "Dotfiles Maintainer"
vendorKey: "j-shilling"
agentKey: "dotfiles"
version: "0.1.0"
slug: "j-shilling/dotfiles"
description: "Personal Linux dotfiles: Emacs, shell, git, and cross-tool agent config via GNU Stow"
author: "@j-shilling"
license: "MIT"
tags: ["dotfiles", "emacs", "stow", "linux"]

skills:
  - name: "gnu-stow"
    source: "local"
    version: "0.1.0"
    required: false
  - name: "stow-dotfiles"
    source: "local"
    version: "0.2.0"
    required: true
  - name: "emacs-config"
    source: "local"
    version: "0.1.0"
    required: false
  - name: "ai-integration"
    source: "local"
    version: "0.1.0"
    required: false

mcpServers:
  - vendor: "j-shilling"
    server: "playwright"
    version: "1.0.0"
    configDir: "mcp-configs/playwright"
    required: false
  - vendor: "j-shilling"
    server: "context7"
    version: "1.0.0"
    configDir: "mcp-configs/context7"
    required: false
  - vendor: "j-shilling"
    server: "filesystem"
    version: "1.0.0"
    configDir: "mcp-configs/filesystem"
    required: false

agents:
  - vendor: "j-shilling"
    agent: "emacs-maintainer"
    version: "0.1.0"
    role: "emacs-config"
    delegations: ["emacs-init", "lisp-packages"]
    required: false
  - vendor: "j-shilling"
    agent: "config-reviewer"
    version: "0.1.0"
    role: "reviewer"
    delegations: ["stow-safety", "git-hygiene"]
    required: false

harnessConfig:
  claude-code:
    progressive-disclosure: true
  opencode:
    config: "agents/.config/opencode/opencode.jsonc"
---

# Agent Purpose

Maintain a personal dotfiles repository that configures development environments across Linux machines. Agents working here should produce **stow-safe, modular, XDG-compliant** changes and respect the separation between shared repo config and machine-local secrets.

This manifest follows [Open Agent Format](https://openagentformat.com/) (OAF v0.8.0). Deep reference material lives in `docs/agents/`.

## Core Responsibilities

- Edit configuration files inside stow packages without breaking symlink deployment
- Keep Emacs config modular (`init-*.el` per domain)
- Follow XDG Base Directory conventions for new paths
- Minimize scope: focused diffs, no unrelated changes
- Never commit secrets, credentials, or machine-specific tokens

## Repository Layout

Stow packages (see [Makefile](Makefile)):

| Package | Deploys to | Primary content |
|---------|------------|-----------------|
| `emacs` | `~/.config/emacs/` | Modular Emacs config, custom lisp, AI prompts |
| `git` | `~/.config/git/` | Git config with conditional includes |
| `shell` | `~/` | bash/zsh, language runtime managers |
| `utils` | `~/.local/bin/` | Utility scripts |
| `agents` | `~/.config/opencode/` | OpenCode harness overlay (not OAF definitions) |
| `claude` | `~/.claude/` | Claude Code user settings |
| `copilot` | `~/.copilot/` | Copilot CLI settings and MCP config |

OAF artifacts at repo root (not stowed): `skills/`, `mcp-configs/`, `subagents/`, `docs/agents/`.

## Build and Deploy

Install all packages:

```bash
make
# equivalent to: stow -d ~/dotfiles -t ~ --no-folding -v -R <pkg> for each package
```

Install or restow a single package:

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -R emacs
```

Remove Emacs backup files:

```bash
~/.local/bin/clean-backups
```

Reload Emacs config: `M-x eval-buffer` in `init.el`, or restart Emacs.

See [docs/agents/stow-and-packages.md](docs/agents/stow-and-packages.md) for ignore rules and package details.

## Capabilities

### Domain Knowledge

- GNU Stow symlink deployment
- Emacs `use-package` modular configuration
- GPTel + MCP AI integration in Emacs
- Git workflow (Magit, conditional work/personal configs)

### Technical Skills

- Emacs Lisp, shell scripting
- Tree-sitter grammars, eglot LSP
- AgentSkills.io and OAF manifest authoring

## Behavioral Guidelines

### Do

- Match existing naming and module organization in surrounding files
- Use `init--state-file` and `init--cache-file` for new Emacs paths
- Store API keys via `password-store`, not in repo files
- Ask before creating git commits or force-pushing

### Don't

- Run `git config` changes
- Force-push to `main`
- Commit `.env`, credentials, or OAuth tokens
- Add files that violate `.stow-local-ignore` without updating ignore rules
- Over-engineer: prefer the smallest correct diff

## Tool Usage Patterns

- **Stow CLI**: activate `gnu-stow` for stow/restow/unstow commands, flags, conflicts, and ignore-list semantics
- **This repo's packages**: activate `stow-dotfiles` when adding files inside stow packages or running `make`
- **Emacs edits**: activate `emacs-config` for `init-*.el` and `lisp/` work
- **AI config**: activate `ai-integration` for `init-ai.el`, MCP, or `prompts/`
- **MCP servers**: use configs in `mcp-configs/`; see [docs/agents/ai-integration.md](docs/agents/ai-integration.md)

## Delegation Strategy

| Condition | Delegate to |
|-----------|-------------|
| Edits under `emacs/.config/emacs/` | `j-shilling/emacs-maintainer` sub-agent |
| Pre-commit review, stow safety checks | `j-shilling/config-reviewer` sub-agent |
| Stow CLI flags, conflicts, unstow/simulate | `gnu-stow` skill |
| Stow install/restow in this repo | `stow-dotfiles` skill |
| Deep Emacs module reference | `docs/agents/emacs-modules.md` |

## Progressive Disclosure

| Topic | Reference |
|-------|-----------|
| Full architecture and directory trees | [docs/agents/architecture.md](docs/agents/architecture.md) |
| Emacs `init-*.el` modules | [docs/agents/emacs-modules.md](docs/agents/emacs-modules.md) |
| Stow packages and ignore rules | [docs/agents/stow-and-packages.md](docs/agents/stow-and-packages.md) |
| GPTel, MCP servers, models | [docs/agents/ai-integration.md](docs/agents/ai-integration.md) |
| Cross-tool harness mapping | [docs/agents/harness-mapping.md](docs/agents/harness-mapping.md) |

## Git Conventions

- **Work directly on `main`** — this is a personal dotfiles repo synced between machines, not a collaborative project; do not create feature branches
- Remote: `origin` (GitHub: j-shilling)
- Editor: `emacsclient --reuse-frame`
- Only commit when explicitly requested

## Version History

- **0.1.0** (2026-06-27): Initial OAF manifest; migrated from CLAUDE.md
