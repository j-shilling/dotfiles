# Stow and Packages

## Skills

| Skill | Path | Use for |
|-------|------|---------|
| `unix-manuals` | [skills/unix-manuals/](../../skills/unix-manuals/) | How to consult `man`/`info` for niche CLI tools |
| `gnu-stow` | [skills/gnu-stow/](../../skills/gnu-stow/) | Portable Stow CLI: flags, restow, unstow, conflicts, ignore lists |
| `stow-dotfiles` | [skills/stow-dotfiles/](../../skills/stow-dotfiles/) | This repo's packages, Makefile, `.stow-local-ignore` |

Deep CLI reference: [skills/gnu-stow/references/stow-cli.md](../../skills/gnu-stow/references/stow-cli.md).

Portability and local overrides: [portability-and-overrides.md](portability-and-overrides.md).

## Quick reference

```bash
# Install all packages
make

# Install/restow one package
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -R <package>

# Available packages
emacs  git  shell  utils  agents  claude  copilot
```

## Makefile variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `DOTFILES_DIR` | `~/dotfiles` | Stow source directory |
| `TARGET` | `$HOME` | Stow target directory |

## Package details

### emacs

Deploys `emacs/.config/emacs/` → `~/.config/emacs/`

### git

Deploys `git/.config/git/` → `~/.config/git/`

On macOS with Homebrew:

```bash
ln -sf "$(pwd)/git/.config/git/config_macos_homebrew" ~/.config/git/config_local
```

### shell

Deploys dotfiles to `~/` (`.bashrc`, `.zshrc`, `.profile`, etc.)

Language runtime managers (PyEnv, NVM, RVM, GHCup, Cargo, pnpm) are sourced here.

### utils

Deploys `utils/.local/bin/` → `~/.local/bin/`

### agents

Deploys `agents/.config/opencode/` → `~/.config/opencode/`

This is the **OpenCode harness overlay**, not OAF agent definitions. OAF manifests live at repo root.

### claude

Deploys `claude/.claude/` → `~/.claude/`

User-level Claude Code settings. Ephemeral files excluded via `.stow-local-ignore`.

### copilot

Deploys `copilot/.copilot/` → `~/.copilot/`

Copilot CLI settings and MCP config. Ephemeral files excluded.

## Ignore rules

Root `.stow-local-ignore` excludes:

- `.git`, `*.org` (source documentation)
- Emacs backups (`*.~*~`, `#*#`)
- Claude ephemeral: `.claude.json`, caches, history, sessions, plugins data
- Copilot ephemeral: config state, logs, session state, OAuth config
- `.aider*` files

`git/.stow-local-ignore` has package-specific rules.

## Adding a new stow package

1. Create a top-level directory mirroring `$HOME` paths
2. Add the package name to `PKGS` in the Makefile
3. Add any needed ignore rules to `.stow-local-ignore`
4. Run `make <package>` or `stow -R ...`

## OAF artifacts (not stowed)

These directories are at repo root and managed by git only:

- `AGENTS.md`, `CLAUDE.md`
- `skills/`, `mcp-configs/`, `subagents/`, `docs/`

Do not place them inside stow packages unless they intentionally target `$HOME`.

## Language runtime setup

Documented in [README.org](../../README.org):

- **PyEnv**: `curl -fsSL https://pyenv.run | bash`
- **NVM**: `curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.2/install.sh | bash`
- **PHP**: php.new installer + phpactor
- **Haskell**: GHCup installer
