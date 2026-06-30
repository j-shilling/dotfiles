# Portability and Local Overrides

This dotfiles repo is shared across **Linux and macOS**, and across **personal and work** contexts. Shared config lives in git; machine- or context-specific values live in local override files that are never committed.

## Design goals

1. **One repo, four contexts** — Linux/macOS × personal/work should all work from the same tree
2. **Local overrides, not forks** — use optional include files instead of branches or duplicate configs
3. **Open, portable standards first** — XDG Base Directory, POSIX shell where shared, OAF/AGENTS.md for agents
4. **POSIX-leaning shell** — prefer POSIX in files sourced everywhere (e.g. `.profile`); bash/zsh extensions are fine in `.bash.d`/`.zsh.d` when needed, but do not assume GNU-only tools or flags without a fallback
5. **Pragmatic, not dogmatic** — GNU/Linux-only paths are acceptable when guarded (`command -v`, `[[ -f ... ]]`, platform checks)

## Context matrix

| | Personal | Work |
|---|----------|------|
| **Linux** | Default git identity (`config_personal`) | `includeIf` → `config_cms` when remote URL matches CMS orgs |
| **macOS** | Same + optional Homebrew git overrides | Same work git rules; brew paths in zsh |

Emacs uses `IS-MAC`, `IS-LINUX`, `IS-WSL` (and `init-lib-*-p` in `init-lib.el`) for platform branches.

## Local override system

Override files are **optional**, **gitignored or stow-ignored**, and **sourced conditionally** from shared config.

| Domain | Shared (committed) | Local override | Notes |
|--------|---------------------|----------------|-------|
| Git | `config`, `config_personal` | `config_local` | Machine-specific (e.g. macOS Homebrew `config_macos_homebrew` → `config_local` symlink) |
| Git (work) | — | `config_cms` | Loaded via `includeIf` on remote URL, not by machine |
| Shell | `.profile`, `.bash.d/*`, `.zsh.d/*` | `secrets.bash`, `secrets.zsh` | Work-only API keys/env; create on machine, never commit |
| Shell | `.profile` | — | Keep POSIX-compliant; sets XDG vars on both platforms |
| Emacs | `init-*.el` | `custom.el` in XDG state dir | Via `(custom-file (init--state-file "custom.el"))` |
| Claude Code | `agents/.claude/settings.json` (stowed) | `agents/.claude/settings.local.json` | Permissions and machine overrides |
| Codex CLI | `agents/.codex/config.toml` (stowed) | `agents/.codex/config.local.toml` | MCP secrets, project trust, work MCP blocks |
| API keys | — | `password-store`, env | Never in repo; `init-ai.el` uses password-store |

### Git local setup (macOS)

```bash
ln -sf "$(pwd)/git/.config/git/config_macos_homebrew" ~/.config/git/config_local
```

### Shell secrets (work machines)

Create locally (not in git):

- `~/.bash.d/secrets.bash` — sourced if present from shared shell init
- `~/.zsh.d/secrets.zsh` — same for zsh

Use for work-only tokens, org-specific env vars, or machine secrets. Personal machines simply omit these files.

### Adding a new override

1. Put shared defaults in the stow package (committed)
2. Add override filename to `.gitignore` and/or package `.stow-local-ignore`
3. Document the override in this file
4. Source with `[ -r "$file" ] && . "$file"` (POSIX) or guarded bash/zsh equivalent
5. Never commit secrets or machine-specific paths into shared files

## POSIX and cross-platform shell

| File | Shell | Rule |
|------|-------|------|
| `.profile` | POSIX `sh` | XDG exports, `uname` cases, PATH — no bashisms |
| `.bashrc`, `.bash.d/*` | bash | OK for bash features; guard optional tools |
| `.zshrc`, `.zsh.d/*` | zsh | OK for zsh features; provide Homebrew and Linux paths (see `tools.zsh` fzf example) |

Prefer:

- `[ -f path ]` / `command -v tool` before invoking platform-specific tools
- `$HOME`, XDG variables, and `uname -s` over hardcoded `/home/...` paths
- `$(uname -s)` with `Darwin` / `Linux` cases in POSIX files

Avoid in shared POSIX paths:

- Bash arrays, `[[ ]]`, `source` (use `.` in `.profile`)
- Assuming `readlink -f`, GNU `sed -i`, or Linux-only `/usr/share/...` without macOS fallback

GNU Stow itself is a deliberate exception — it is required for deployment on all platforms this repo targets.

## Open standards used

| Standard | Where |
|----------|-------|
| XDG Base Directory | `.profile`, Emacs `init--state-file` / `init--cache-file` |
| GNU Stow | Package deployment |
| Open Agent Format | `AGENTS.md`, skills, subagents |
| AgentSkills.io | `skills/*/SKILL.md` |
| password-store | API keys (Emacs, git signing via SSH key) |
| Conditional git config | Work/personal identity split |

Prefer standards with broad tool support (XDG, POSIX, OAF) over vendor-specific config when adding new surface area.

## Agent checklist

When changing config, verify:

- [ ] Works on Linux and macOS (or platform guard is explicit)
- [ ] No secrets in committed files
- [ ] Machine-specific values go in a documented override file
- [ ] Work-only values use `secrets.*` or `config_cms`, not shared defaults
- [ ] Shared shell snippets in `.profile` remain POSIX-compliant
- [ ] New paths use XDG (`XDG_CONFIG_HOME`, `init--state-file`, etc.)
