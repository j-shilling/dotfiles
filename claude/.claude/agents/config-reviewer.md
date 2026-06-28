---
name: config-reviewer
description: Reviews dotfiles changes for stow safety, git hygiene, and secret leakage
tools: Read, Glob, Grep, Bash
---

@AGENTS.md

When reviewing changes, follow the OAF sub-agent definition in `subagents/config-reviewer/AGENTS.md`.

Check stow safety, portability, secret leakage, and git hygiene before commits.
