# Architecture

Personal dotfiles repository managed with GNU Stow. Configurations are organized as stow packages that symlink into `$HOME`.

## Top-level layout

```
dotfiles/
├── AGENTS.md              # OAF canonical agent manifest
├── CLAUDE.md              # @AGENTS.md bridge for Claude Code
├── Makefile               # stow all packages
├── skills/                # OAF local skills (not stowed)
├── mcp-configs/           # OAF MCP server configs (not stowed)
├── subagents/             # OAF sub-agent definitions (not stowed)
├── docs/agents/           # Progressive disclosure reference (not stowed)
├── emacs/                 # stow → ~/.config/emacs/
├── git/                   # stow → ~/.config/git/
├── shell/                 # stow → ~/ (.bashrc, .zshrc, direnv, gpg-agent, etc.)
├── mail/                  # stow → ~/.config/{notmuch,msmtp,isync}/
├── ssh/                   # stow → ~/.ssh/config
├── utils/                 # stow → ~/.local/bin/
├── agents/                # stow → ~/.claude/, ~/.codex/, ~/.copilot/, ~/.cursor/,
│                         #         ~/.config/agents/, ~/.config/opencode/, ~/.config/Cursor/User/
```

## Emacs configuration

`emacs/.config/emacs/` — modular Emacs setup:

| Path | Purpose |
|------|---------|
| `init.el` | Entry point, loads modules |
| `init-lib.el` | XDG path helpers |
| `init-package.el` | use-package, MELPA, completion |
| `init-appearance.el` | Themes, fonts, UI |
| `init-editing.el` | Text editing |
| `init-prog.el` | Programming language modes |
| `init-tools.el` | Eshell, dired, Magit |
| `init-org.el` | Org-mode |
| `init-ai.el` | GPTel, MCP integration |
| `init-ai-tools.el` | GPTel tool definitions |
| `init-tree-sitter.el` | Tree-sitter grammars |
| `init-completion.el` | Completion framework |
| `lisp/` | Custom packages (markdown-ts-mode, terraform-ts-mode, erb-ts-mode, gptel-*) |
| `prompts/` | AI assistant prompt templates |
| `templates/` | Code snippet templates |

## Git configuration

`git/.config/git/` — conditional includes:

- `config` — main settings
- `config_personal` — personal identity (default)
- `config_cms` — work context (CMS GitHub orgs, via `includeIf`)
- `config_macos_homebrew` — macOS Homebrew overrides (symlinked as `config_local`)
- `config_local` — machine-specific (not in repo; created per machine)

See [portability-and-overrides.md](portability-and-overrides.md) for the Linux/macOS × personal/work model.

## Shell configuration

`shell/` — bash and zsh with language runtime managers:

- PyEnv (Python), NVM (Node.js), RVM (Ruby), GHCup (Haskell), Cargo (Rust), pnpm
- direnv hooks, XDG user dirs, GPG agent config, `.mailcap`

Runtime managers are sourced in `.bashrc` / `.zshrc`, not `.profile`.

## Email system

`mail/.config/` — mbsync, notmuch, msmtp:

- **mbsync** (isync) — Gmail → local Maildir at `~/mail/`
- **notmuch** — indexing and search
- **msmtp** — SMTP via Gmail with password-store

## SSH configuration

`ssh/.ssh/config` — host aliases and identity files. Private keys and `known_hosts` stay machine-local.

## XDG Base Directory compliance

Emacs state and cache paths:

- State: `~/.local/state/emacs/VERSION/`
- Cache: `~/.cache/emacs/VERSION/`
- Helpers: `init--state-file`, `init--cache-file` in `init.el`

## Utility scripts

`utils/.local/bin/`:

- `clean-backups` — removes Emacs backup files (`*.~*~`)
- `update-ca-bundle` — CA certificate bundle updates

## Building Emacs from source

Required for native compilation, tree-sitter, and PGTK support:

```bash
CFLAGS='-march=native -O3' \
CC=gcc-14 \
../configure --with-native-compilation=aot \
      --with-tree-sitter \
      --with-gif \
      --with-png \
      --with-jpeg \
      --with-rsvg \
      --with-tiff \
      --with-imagemagick \
      --with-pgtk \
      --with-mailutils \
      --with-modules --prefix=$HOME/.local
```

Build dependencies: build-essentials, autoconf, texinfo, libmagickwand-dev, libmagick++-dev, imagemagick, libgccjit0, libgccjit-14-dev, libwebkit2gtk-4.1-dev
