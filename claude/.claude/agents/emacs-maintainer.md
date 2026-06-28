---
name: emacs-maintainer
description: Specialized agent for editing modular Emacs configuration in the dotfiles repo
tools: Read, Edit, Glob, Grep, Bash
---

@AGENTS.md

When working on Emacs config, follow the OAF sub-agent definition in `subagents/emacs-maintainer/AGENTS.md` and activate the `emacs-config` skill.

Scope: changes under `emacs/.config/emacs/` only. Restow after edits: `stow -R -d ~/dotfiles -t ~ --no-folding -v emacs`.
