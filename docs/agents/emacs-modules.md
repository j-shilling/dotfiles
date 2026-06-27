# Emacs Modules

Per-file responsibilities for `emacs/.config/emacs/`.

## init.el

Main entry point. Loads all `init-*.el` modules. Defines XDG helpers `init--state-file` and `init--cache-file`.

## init-lib.el

Utility functions for XDG-compliant path handling. Use these for any new state or cache file paths.

## init-package.el

- `use-package` declarations and MELPA setup
- Completion framework: vertico, orderless, marginalia, consult, corfu, embark

## init-appearance.el

Themes, fonts, UI customization.

## init-editing.el

General text editing configuration (movement, indentation, etc.).

## init-prog.el

Programming language support:

| Language | Tooling |
|----------|---------|
| Ruby | rvm, inf-ruby, robe, rbs-mode |
| JavaScript/TypeScript | nvm, web-mode |
| Terraform | terraform-mode, terraform-ts-mode |
| Markdown | markdown-ts-mode (custom) |
| ERB | erb-ts-mode (custom) |
| PHP | eglot + phpactor |
| Haskell | GHCup |
| Common Lisp | Roswell |

LSP via `eglot` (Emacs 29+).

## init-tools.el

- Eshell
- dired
- Magit (git porcelain), magit-todos, forge (GitHub/GitLab), diff-hl

## init-org.el

Org-mode configuration.

## init-ai.el

GPTel backends and MCP hub. See [ai-integration.md](ai-integration.md).

## init-ai-tools.el

GPTel tool definitions:

- Buffer operations (`gptel-buffers`)
- Filesystem operations (`gptel-filesystem`)
- Diff/patch tools (`create_patch_buffer`, etc.)

## init-tree-sitter.el

Tree-sitter grammar setup for supported languages.

## init-completion.el

Additional completion framework configuration.

## Custom lisp packages

`lisp/` directory:

- `markdown-ts-mode` — Markdown with tree-sitter
- `terraform-ts-mode` — Terraform with tree-sitter
- `erb-ts-mode` — ERB templates with tree-sitter
- `gptel-filesystem`, `gptel-buffers`, `gptel-info`, `gptel-xref`, `gptel-project` — GPTel extensions

## Prompts

`prompts/` — reusable AI assistant templates:

- `dev-work-planner.md`
- `effect-backend.md`
- `gptel-tool-definition-assistant.md`
- `mermaid-diagram-assistant.md`
- `prompt-generator.md`

## Patterns for new code

1. One domain per `init-*.el` file
2. `use-package` with `:defer`, `:hook`, or `:commands`
3. XDG paths via `init--state-file` / `init--cache-file`
4. Custom packages in `lisp/`, required from the appropriate init module

## Emacs development commands

```bash
emacs                          # launch
emacsclient -c                 # connect to daemon
M-x package-install RET <pkg>  # install package
M-x package-update-all         # update all packages
M-x eval-buffer                # reload current buffer
```
