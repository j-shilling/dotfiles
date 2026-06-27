---
name: "emacs-config"
description: >
  Edit modular Emacs configuration in emacs/.config/emacs/. Use when changing
  init-*.el modules, use-package setup, custom lisp packages, tree-sitter
  grammars, XDG path helpers, or Emacs appearance/editing/prog settings — even
  if the user only mentions "Emacs config" or "init.el".
license: "MIT"
metadata:
  author: "j-shilling"
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# Emacs Configuration

## Module organization

Configuration is split into focused `init-*.el` files loaded from [init.el](emacs/.config/emacs/init.el):

| Module | Domain |
|--------|--------|
| `init-lib.el` | XDG path helpers (`init--state-file`, `init--cache-file`) |
| `init-package.el` | use-package, MELPA, completion (vertico, consult, corfu) |
| `init-appearance.el` | Themes, fonts, UI |
| `init-editing.el` | Text editing |
| `init-prog.el` | Programming language modes, eglot LSP |
| `init-tools.el` | Eshell, dired, Magit |
| `init-org.el` | Org-mode |
| `init-ai.el` | GPTel, MCP hub |
| `init-ai-tools.el` | GPTel tool definitions |
| `init-tree-sitter.el` | Tree-sitter grammars |
| `init-completion.el` | Completion framework details |

Add new domains as new `init-*.el` files; require them from `init.el`.

## Patterns

1. Wrap packages in `use-package` with appropriate `:defer`, `:hook`, or `:commands`
2. Use `init--state-file` and `init--cache-file` for any new file paths (XDG compliance)
3. Custom packages go in [lisp/](emacs/.config/emacs/lisp/) (markdown-ts-mode, terraform-ts-mode, erb-ts-mode, gptel-*)
4. AI prompt templates go in [prompts/](emacs/.config/emacs/prompts/)
5. Code snippet templates go in [templates/](emacs/.config/emacs/templates/)
6. Platform branches: use `IS-MAC`, `IS-LINUX`, `IS-WSL` or `init-lib-*-p` — support Linux and macOS

## Portability

- Prefer XDG paths (`init--state-file`, `init--cache-file`) over hardcoded locations
- See [docs/agents/portability-and-overrides.md](docs/agents/portability-and-overrides.md) for cross-platform and local override patterns

## After editing

Restow the emacs package:

```bash
stow -R -d ~/dotfiles -t ~ --no-folding -v emacs
```

Reload in running Emacs: `M-x eval-buffer` on the changed file, or restart.

## Deep reference

See [docs/agents/emacs-modules.md](docs/agents/emacs-modules.md) for per-module details, LSP setup, and language support.
