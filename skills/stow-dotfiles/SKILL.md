---
name: "stow-dotfiles"
description: >
  Install and restow GNU Stow dotfile packages in this repository. Use when
  adding files inside stow packages, deploying config to $HOME, running make,
  updating symlinks, or checking .stow-local-ignore rules — even if the user
  only says "install dotfiles" or "deploy config".
license: "MIT"
metadata:
  author: "j-shilling"
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# Stow Dotfiles

## Package inventory

Stow packages defined in the [Makefile](Makefile):

```
emacs  git  shell  utils  agents  claude  copilot
```

Each package is a top-level directory whose contents mirror paths under `$HOME`.

## Install all packages

```bash
make
```

`DOTFILES_DIR` defaults to `~/dotfiles`; `TARGET` defaults to `$HOME`.

## Install or restow one package

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -R <package>
```

Use `-R` to restow after adding or renaming files inside a package.

## Ignore rules

Root [.stow-local-ignore](.stow-local-ignore) excludes:

- `.git`, `*.org` source docs
- Emacs backup patterns (`*.~*~`, `#*#`)
- Claude/Copilot ephemeral files and caches
- `.aider*` files

Package-level ignores exist (e.g. [git/.stow-local-ignore](git/.stow-local-ignore)).

Before adding a new file inside a stow package, confirm it will not be excluded. Update ignore rules only when intentional.

## Adding a new file inside a package

1. Place the file at the correct path inside the package directory (e.g. `emacs/.config/emacs/init-foo.el`)
2. Restow: `stow -R -d ~/dotfiles -t ~ --no-folding -v emacs`
3. Verify the symlink at the target path

## OAF artifacts (not stowed)

These live at repo root and are **not** symlinked by stow:

- `AGENTS.md`, `CLAUDE.md`, `skills/`, `mcp-configs/`, `subagents/`, `docs/`

Do not move OAF definitions inside stow packages unless they target `$HOME`.
