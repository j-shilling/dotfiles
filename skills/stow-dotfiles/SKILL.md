---
name: "stow-dotfiles"
description: >
  Deploy and restow stow packages in this dotfiles repository. Use when editing
  files inside emacs/, git/, shell/, mail/, ssh/, utils/, agents/, claude/, or copilot/
  packages, running make, checking package .stow-local-ignore files, or adding a new stow
  package — even if the user only says "install dotfiles" or "deploy config".
license: "MIT"
metadata:
  author: "j-shilling"
  version: "0.2.0"
allowed-tools: ["bash", "read", "edit"]
---

# Stow Dotfiles (this repo)

For GNU Stow CLI semantics, flags, conflicts, and ignore-list rules, read the **`gnu-stow`** skill (`skills/gnu-stow/SKILL.md`).

## Canonical command

Matches the [Makefile](Makefile):

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -R <package>
```

Install all packages:

```bash
make
```

Variables: `DOTFILES_DIR` (default `~/dotfiles`), `TARGET` (default `$HOME`).

## Packages

| Package | Deploys to |
|---------|------------|
| `emacs` | `~/.config/emacs/` |
| `git` | `~/.config/git/` |
| `shell` | `~/` (`.bashrc`, `.zshrc`, `.mailcap`, direnv, gpg-agent, etc.) |
| `mail` | `~/.config/{notmuch,msmtp,isync}/` |
| `ssh` | `~/.ssh/config` |
| `utils` | `~/.local/bin/` |
| `agents` | `~/.config/opencode/` |
| `claude` | `~/.claude/` |
| `copilot` | `~/.copilot/` |

## Workflow: add or edit a file

1. Place file at the correct path inside the package (e.g. `emacs/.config/emacs/init-foo.el`)
2. Confirm it is not excluded by the package's `.stow-local-ignore` if one exists
3. Restow: `stow -d ~/dotfiles -t ~ --no-folding -v -R <package>`
4. Verify: `readlink <target-path>`

## Ignore rules

Package-level `.stow-local-ignore` files exclude machine-local or ephemeral files: `git/`, `shell/` (`secrets.bash`/`secrets.zsh`), `ssh/` (private keys, `known_hosts`), `claude/`, `copilot/`.

## Local overrides

Machine- and work-specific files are never committed. See [portability-and-overrides.md](../portability-and-overrides.md).

| Override | Location |
|----------|----------|
| Shell secrets | `~/.bash.d/secrets.bash`, `~/.zsh.d/secrets.zsh` (create locally; sourced from `security.*`) |
| Git machine config | `~/.config/git/config_local` |
| Git work identity | `config_cms` via `includeIf` on remote URL |
| GitHub HTTPS via `gh` | `config_github_https_gh` included or symlinked from `config_local` |

## macOS git exception

```bash
ln -sf "$(pwd)/git/.config/git/config_macos_homebrew" ~/.config/git/config_local
```

## GitHub HTTPS via gh

For machines where GitHub SSH is blocked:

```bash
ln -sf "$(pwd)/git/.config/git/config_github_https_gh" ~/.config/git/config_local
gh auth login --hostname github.com
```

## Not stowed (repo root)

`AGENTS.md`, `CLAUDE.md`, `skills/`, `mcp-configs/`, `subagents/`, `docs/` — git-managed only, not symlinked to `$HOME`.

## Adding a new package

1. Create top-level directory mirroring `$HOME` paths
2. Add name to `PKGS` in [Makefile](Makefile)
3. Add a package-level `.stow-local-ignore` if needed
4. `make <package>`

See [docs/agents/stow-and-packages.md](docs/agents/stow-and-packages.md) for details.
