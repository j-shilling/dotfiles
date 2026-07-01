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
emacs  git  shell  mail  ssh  utils  agents
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

On machines where GitHub SSH is blocked, use GitHub CLI over HTTPS:

```bash
ln -sf "$(pwd)/git/.config/git/config_github_https_gh" ~/.config/git/config_local
gh auth login --hostname github.com
```

### shell

Deploys dotfiles to `~/` (`.bashrc`, `.zshrc`, `.profile`, `.mailcap`, `.config/direnv/`, `.config/user-dirs.*`, `.gnupg/gpg-agent.conf`, etc.)

Language runtime managers (PyEnv, NVM, RVM, GHCup, Cargo, pnpm) are sourced here.

### mail

Deploys `mail/.config/{notmuch,msmtp,isync}/` → `~/.config/notmuch/`, `~/.config/msmtp/`, `~/.config/isync/`

Email stack: mbsync (isync) → local Maildir, notmuch indexing, msmtp SMTP via password-store.

### ssh

Deploys `ssh/.ssh/config` → `~/.ssh/config`

Private keys and `known_hosts` are excluded via `ssh/.stow-local-ignore` (machine-local).

### utils

Deploys `utils/.local/bin/` → `~/.local/bin/`

### agents

Deploys into multiple target directories:

| Target | Content |
|--------|---------|
| `~/.claude/` | Claude Code user settings (`settings.json`, `CLAUDE.md`) |
| `~/.codex/` | Codex global guidance (`AGENTS.md`); runtime config stays local |
| `~/.copilot/` | Copilot CLI settings and MCP config (`settings.json`, `mcp-config.json`) |
| `~/.cursor/` | Cursor permissions (`permissions.json`) |
| `~/.config/agents/` | User-level OAF directory (`AGENTS.md`, skills, subagents, MCP config symlinks) |
| `~/.config/opencode/` | OpenCode harness overlay (`opencode.jsonc`) |
| `~/.config/Cursor/User/` | Cursor editor settings (`settings.json`) |

Consolidated package replacing the old separate `claude`, `codex`, `copilot`,
and `cursor` packages, plus OpenCode from the original `agents` package.

The Codex portion is intentionally narrow: it deploys
`agents/.codex/AGENTS.md` to `~/.codex/AGENTS.md`. The live
`~/.codex/config.toml`, profile files, auth, sessions, caches, app state,
project trust, and plugin state are machine-local and excluded from Stow. Do
not use `--adopt` for Codex config or state on the work laptop. User-level
portable skills and plugin marketplace metadata belong under `~/.agents` when
present.

## Ignore rules

Package-level `.stow-local-ignore` files exclude machine-local or ephemeral files:

- `git/.stow-local-ignore` — package-specific git rules
- `shell/.stow-local-ignore` — `secrets.bash`, `secrets.zsh`
- `ssh/.stow-local-ignore` — private keys, `known_hosts`
- `agents/.stow-local-ignore` — ephemeral harness state for all consolidated agent configs

## Adding a new stow package

1. Create a top-level directory mirroring `$HOME` paths
2. Add the package name to `PKGS` in the Makefile
3. Add any needed ignore rules to a package-level `.stow-local-ignore`
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
