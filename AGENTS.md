---
name: "Dotfiles Maintainer"
vendorKey: "j-shilling"
agentKey: "dotfiles"
version: "0.1.0"
slug: "j-shilling/dotfiles"
description: "Personal dotfiles: Emacs, shell, git, and cross-tool agent config via GNU Stow (Linux and macOS)"
author: "@j-shilling"
license: "MIT"
tags: ["dotfiles", "emacs", "stow", "linux", "macos"]

skills:
  - name: "unix-manuals"
    source: "local"
    version: "0.1.0"
    required: false
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

Maintain a personal dotfiles repository that configures development environments across **Linux and macOS**, for both **personal and work** projects. Agents working here should produce **stow-safe, modular, XDG-compliant** changes, respect **local override files** for secrets and machine-specific settings, and prefer **open, cross-platform standards** (POSIX-leaning shell in shared paths; pragmatic use of platform tools when guarded).

This manifest follows [Open Agent Format](https://openagentformat.com/) (OAF v0.8.0). Deep reference material lives in `docs/agents/`.

## Core Responsibilities

- Edit configuration files inside stow packages without breaking symlink deployment
- Keep Emacs config modular (`init-*.el` per domain)
- Follow XDG Base Directory conventions for new paths
- Support Linux, macOS, personal, and work contexts from one repo via local overrides
- Minimize scope: focused diffs, no unrelated changes
- Never commit secrets, credentials, or machine-specific tokens

## Portability and Overrides

Shared config is committed; local overrides are optional and never committed.

| Context | Mechanism |
|---------|-----------|
| Linux / macOS | Platform checks (`IS-MAC`, `uname`, guarded paths); see Emacs `init-lib.el`, shell `.profile` |
| Personal / work git | `config_personal` + conditional `includeIf` → `config_cms` |
| Machine-specific git | `config_local` (e.g. macOS Homebrew symlink) |
| Work-only shell secrets | `secrets.bash` / `secrets.zsh` in `~/.bash.d` / `~/.zsh.d` (local only) |
| Emacs UI prefs | `custom.el` in XDG state dir |
| Claude permissions | `settings.local.json` |

**POSIX-leaning shell**: keep `.profile` POSIX-compliant; bash/zsh extensions belong in `.bash.d`/`.zsh.d`. Prefer portable constructs in shared code; do not assume GNU extensions without fallbacks or guards — but this is a guideline, not a hard rule.

See [docs/agents/portability-and-overrides.md](docs/agents/portability-and-overrides.md) for the full override catalog and agent checklist.

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
- Store API keys via `password-store` or local `secrets.*` override files, not in repo files
- Put machine- or work-only values in documented override files (`config_local`, `secrets.bash`, etc.)
- Prefer XDG, POSIX (in `.profile`), and guarded cross-platform paths over hardcoded Linux assumptions
- Consult `man`/`info` (via `unix-manuals`) for niche CLI tools before guessing flags
- Ask before creating git commits or force-pushing

### Don't

- Run `git config` changes
- Force-push to `main`
- Commit `.env`, credentials, OAuth tokens, or work-only secrets to shared files
- Hardcode `/home/...` paths or Linux-only tool paths without macOS fallback or guards
- Add files that violate `.stow-local-ignore` without updating ignore rules
- Over-engineer: prefer the smallest correct diff

## Tool Usage Patterns

- **Local CLI docs**: activate `unix-manuals` and consult `man`/`info` before guessing flags on niche tools
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
| Unknown CLI flags, niche tool behavior | `unix-manuals` skill |
| Deep Emacs module reference | `docs/agents/emacs-modules.md` |

## Progressive Disclosure

| Topic | Reference |
|-------|-----------|
| Full architecture and directory trees | [docs/agents/architecture.md](docs/agents/architecture.md) |
| Emacs `init-*.el` modules | [docs/agents/emacs-modules.md](docs/agents/emacs-modules.md) |
| Stow packages and ignore rules | [docs/agents/stow-and-packages.md](docs/agents/stow-and-packages.md) |
| GPTel, MCP servers, models | [docs/agents/ai-integration.md](docs/agents/ai-integration.md) |
| Cross-tool harness mapping | [docs/agents/harness-mapping.md](docs/agents/harness-mapping.md) |
| Portability, POSIX, local overrides | [docs/agents/portability-and-overrides.md](docs/agents/portability-and-overrides.md) |

## Git Conventions

- **Work directly on `main`** — this is a personal dotfiles repo synced between machines, not a collaborative project; do not create feature branches
- Remote: `origin` (GitHub: j-shilling)
- Editor: `emacsclient --reuse-frame`
- Only commit when explicitly requested

## Version History

- **0.1.0** (2026-06-27): Initial OAF manifest; migrated from CLAUDE.md
