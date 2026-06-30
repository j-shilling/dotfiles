---
name: "Jake's User-Level Agents"
vendorKey: "j-shilling"
agentKey: "user-agents"
version: "0.1.0"
slug: "j-shilling/user-agents"
description: "User-level OAF agents, skills, and MCP configs for personal AI harness tooling"
author: "@j-shilling"
license: "MIT"
tags: ["dotfiles", "personal", "user-level", "oaf"]

skills:
  - name: "create-oaf-agent"
    source: "local"
    version: "0.1.0"
    required: false
  - name: "create-agent-skill"
    source: "local"
    version: "0.1.0"
    required: false

agents:
  - vendor: "j-shilling"
    agent: "emacs-maintainer"
    version: "0.1.0"
    role: "emacs-config"
    required: false
  - vendor: "j-shilling"
    agent: "config-reviewer"
    version: "0.1.0"
    role: "reviewer"
    required: false

mcpServers:
  - vendor: "j-shilling"
    server: "playwright"
    version: "1.0.0"
    configDir: "mcp-configs/playwright"
    required: false
  - vendor: "j-shilling"
    server: "context7"
    version: "1.0.0"
    configDir: "mcp-configs/context7"
    required: false
  - vendor: "j-shilling"
    server: "filesystem"
    version: "1.0.0"
    configDir: "mcp-configs/filesystem"
    required: false

harnessConfig:
  opencode:
    skills_dir: "~/.config/agents/skills/"
    agents_dir: "~/.config/agents/agents/"
  claude-code:
    discovery: "Imported via @~/.config/agents/AGENTS.md in ~/.claude/CLAUDE.md"
  codex:
    discovery: "AGENTS.md is discovered natively; skills can reference ~/.config/agents/skills/"
  cursor:
    discovery: "AGENTS.md is read from project root; user-level ~/.config/agents/AGENTS.md is a supplemental discovery source"
---

# User-Level OAF Agent Configuration

This directory is the canonical user-level Open Agent Format (OAF) configuration,
deployed via GNU Stow from `agents/` package in the dotfiles repo.

## Composition with Project-Level AGENTS.md

The root `AGENTS.md` at `$HOME/dotfiles/AGENTS.md` is the project-level OAF manifest
for the dotfiles repo. This user-level config at `~/.config/agents/AGENTS.md` provides:

- **Meta-skills**: Skills for creating OAF agents and AgentSkills.io skill definitions.
  These are user-level capabilities, not project-specific.
- **Sub-agent references**: Pointers to the canonical sub-agent definitions in the
  dotfiles repo's `subagents/` directory.
- **MCP server configs**: Pointers to the canonical MCP server configs in the
  dotfiles repo's `mcp-configs/` directory.

Each harness discovers this config differently (see `harnessConfig` in the frontmatter).

## Why User-Level OAF?

OAF is designed to work across harnesses. Having a user-level OAF directory means:

1. **One set of skills** shared by all harnesses (no duplication)
2. **One set of MCP configs** shared across all tools
3. **One set of sub-agent definitions** composed from the canonical repo sources
4. **Harness-agnostic authoring** — write once, use from any tool

## Directory Structure

```
~/.config/agents/
├── AGENTS.md              ← This manifest
├── README.md
├── skills/                ← User-level skills (harness-agnostic)
│   ├── create-oaf-agent/SKILL.md
│   └── create-agent-skill/SKILL.md
├── subagents/             ← Symlinks to repo root subagents/
│   ├── emacs-maintainer/ → ../../subagents/emacs-maintainer/
│   └── config-reviewer/  → ../../subagents/config-reviewer/
└── mcp-configs/           ← Symlinks to repo root mcp-configs/
    ├── playwright/
    ├── context7/
    └── filesystem/
```