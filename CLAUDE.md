# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository that manages development environment configuration across Linux systems. The repository uses **GNU Stow** for installing dotfiles via symlinks and focuses heavily on Emacs as the primary development environment with extensive AI integration.

## Installation and Setup

### Stow Installation
To deploy configuration changes:
```bash
stow -d /home/jake/dotfiles/ -t "${HOME}" --no-folding -v emacs
```

The `.stow-local-ignore` file excludes:
- `.git` directory
- `*.org` files (source documentation)
- Emacs backup/temp files (`*.~*~`, `#*#`)

### Building Emacs from Source
When building Emacs (required for native compilation, tree-sitter, and PGTK support):
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
      --with-modules --prefix=/home/jake/.local
```

**Required build dependencies**: build-essentials, autoconf, texinfo, libmagickwand-dev, libmagick++-dev, imagemagick, libgccjit0, libgccjit-14-dev, libwebkit2gtk-4.1-dev

## Code Architecture

### Directory Structure

- **`emacs/.config/emacs/`** - Modular Emacs configuration split into focused files:
  - `init.el` - Main entry point, loads other modules
  - `init-lib.el` - Utility functions for XDG-compliant path handling
  - `init-package.el` - Package management (use-package, MELPA)
  - `init-appearance.el` - Themes, fonts, UI
  - `init-editing.el` - Text editing configuration
  - `init-prog.el` - Programming language modes
  - `init-tools.el` - Eshell, dired, git (Magit)
  - `init-org.el` - Org-mode configuration
  - `init-ai.el` - GPTel and MCP integration for Claude AI
  - `init-ai-tools.el` - AI tool definitions for filesystem operations
  - `init-tree-sitter.el` - Tree-sitter grammar setup
  - `lisp/` - Custom Emacs Lisp packages (markdown-ts-mode, terraform-ts-mode, erb-ts-mode, gptel-filesystem)
  - `prompts/` - AI assistant prompt templates
  - `templates/` - Code snippet templates

- **`.config/git/`** - Git configuration with conditional includes:
  - `config` - Main git settings
  - `config_personal` - Personal git settings (included by default)
  - Conditional work configuration for FunctorFactory context

- **`.config/msmtp/`** - SMTP configuration for email (Gmail with password-store)
- **`.config/direnv/`** - Directory-local environment with Guix and NVM integration
- **`.config/isync/`** - Mail synchronization (mbsync)
- **`.config/notmuch/`** - Email indexing with pre/post-new hooks

- **`utils/.local/bin/`** - Utility scripts:
  - `clean-backups` - Removes Emacs backup files (`*.~*~`)

### XDG Base Directory Compliance

The Emacs configuration strictly follows XDG Base Directory specifications:
- State files: `~/.local/state/emacs/VERSION/`
- Cache files: `~/.cache/emacs/VERSION/`
- Helper functions: `init--state-file` and `init--cache-file` in `init.el`

### Language Runtime Managers

Multiple language version managers are integrated in `.bashrc`:
- **PyEnv** - Python version management
- **NVM** - Node.js version management
- **RVM** - Ruby version management
- **GHCup** - Haskell toolchain
- **Cargo** - Rust packages
- **pnpm** - Node package manager

All are automatically sourced and evaluated on shell startup.

### AI Integration Architecture

The Emacs setup includes deep Claude AI integration:

**GPTel Configuration** (`init-ai.el`):
- Backend: Anthropic Claude API
- Models: claude-sonnet-4-5-20250929 (default), claude-opus-4-1-20250805
- API key management via `password-store`
- Temperature: 0.0 (deterministic)
- Caching enabled
- Tool use enabled with filesystem and buffer tools

**Model Context Protocol (MCP)** servers configured:
- `playwright` - Browser automation
- `mermaid` - Diagram generation
- `filesystem` - File operations on `$HOME`

**Custom AI Tools** (`init-ai-tools.el`):
- Filesystem operations (read, write, list, search, replace)
- Buffer management
- Integrated with GPTel for tool-use workflows

**AI Prompts** (`prompts/`):
- Development work planner
- Effect.js backend assistant
- GPTel tool definition assistant
- Mermaid diagram assistant
- Prompt generator

### Git Workflow

**Configuration highlights**:
- Editor: `emacsclient --reuse-frame`
- Merge: histogram algorithm, no fast-forward, rename detection
- Pull: rebase with ff=false
- Commit signing: disabled
- GitHub user: j-shilling

**Magit integration** in Emacs:
- `magit` - Git porcelain
- `magit-todos` - TODO scanning
- `forge` - GitHub/GitLab integration
- `diff-hl` - Diff highlighting in buffers

### Programming Language Support

Languages configured in Emacs via `init-prog.el` and tree-sitter:
- **Ruby** - rvm, inf-ruby, robe, rbs-mode
- **JavaScript/TypeScript** - nvm integration, web-mode
- **Terraform** - terraform-mode, terraform-ts-mode
- **Markdown** - markdown-ts-mode
- **ERB** - erb-ts-mode (custom)
- **PHP** - LSP via phpactor
- **Haskell** - via GHCup
- **Common Lisp** - via Roswell

LSP integration via `eglot` (built-in to Emacs 29+).

### Completion and Navigation

**Completion framework** (`init-package.el`):
- `vertico` - Minibuffer completion UI
- `orderless` - Flexible completion style
- `marginalia` - Rich annotations
- `consult` - Enhanced commands
- `corfu` - In-buffer completion
- `embark` - Context actions

### Email System

Email workflow with local IMAP sync:
- **mbsync** (isync) - Syncs Gmail to local Maildir
- **notmuch** - Email indexing and search
- **msmtp** - SMTP sending via Gmail
- **Pre/post-new hooks** - Automatic processing on mail sync

## Development Commands

### Emacs Development
- Launch Emacs: `emacs` (or `emacsclient -c` if daemon running)
- Reload configuration: `M-x eval-buffer` in init.el or restart Emacs
- Install packages: `M-x package-install RET <package-name>`
- Update packages: `M-x package-update-all`

### Utility Scripts
```bash
# Remove all Emacs backup files recursively
~/.local/bin/clean-backups
```

### Language-Specific Setup

**Python** (via PyEnv):
```bash
curl -fsSL https://pyenv.run | bash
# Dependencies listed in README.org
```

**Node.js** (via NVM):
```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.2/install.sh | bash
```

**PHP**:
```bash
/bin/bash -c "$(curl -fsSL https://php.new/install/linux/8.4)"
curl -Lo phpactor.phar https://github.com/phpactor/phpactor/releases/latest/download/phpactor.phar
chmod a+x phpactor.phar
mv phpactor.phar ~/.local/bin/phpactor
```

**Haskell** (via GHCup):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## Key Patterns

### When Working with Emacs Configuration

1. **Module organization**: Each `init-*.el` file handles a specific domain (appearance, editing, programming, AI, etc.)
2. **XDG compliance**: Use `init--state-file` and `init--cache-file` for any new file paths
3. **Lazy loading**: Wrap package config in `use-package` with appropriate `:defer`, `:hook`, or `:commands`
4. **Custom Lisp**: Add new Emacs Lisp packages to `lisp/` directory

### When Working with Dotfiles

1. **Stow compatibility**: Ensure new files respect `.stow-local-ignore` patterns
2. **Git configuration**: Personal vs work configs are separated via conditional includes
3. **PATH management**: Language tooling is sourced in `.bashrc`, not `.profile`

### When Working with AI Integration

1. **API keys**: Use `password-store` for secure credential management
2. **Tool definitions**: Define tools in `init-ai-tools.el` following gptel-tool patterns
3. **MCP servers**: Configure new MCP servers in `mcp-hub-servers` alist in `init-ai.el`
4. **Prompts**: Add reusable prompts to `prompts/` directory

## Git Branch Information

- Main branch: `main`
- Remote: origin (GitHub: j-shilling)
