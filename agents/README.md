# Agents Stow Package

This stow package deploys the **OpenCode harness overlay** to `~/.config/opencode/`.

It is **not** where OAF agent definitions live. Those are at the repository root.

## What this package contains

| Path (in repo) | Deploys to | Purpose |
|----------------|------------|---------|
| `.config/opencode/opencode.jsonc` | `~/.config/opencode/opencode.jsonc` | OpenCode provider, MCP, agent tool policies |
| `.config/opencode/skills/create-oaf-agent/` | `~/.config/opencode/skills/...` | Meta-skill for scaffolding OAF agents |
| `.config/opencode/skills/create-agent-skill/` | `~/.config/opencode/skills/...` | Meta-skill for AgentSkills.io authoring |

## Installation

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v agents
# or: make agents
```

## OAF definitions (repo root, not stowed)

Cross-tool agent configuration lives outside this package:

| Path | Purpose |
|------|---------|
| [AGENTS.md](../AGENTS.md) | OAF v0.8.0 canonical manifest |
| [CLAUDE.md](../CLAUDE.md) | `@AGENTS.md` bridge for Claude Code |
| [skills/](../skills/) | Local AgentSkills.io skills |
| [mcp-configs/](../mcp-configs/) | OAF MCP server configs |
| [subagents/](../subagents/) | OAF sub-agent definitions |
| [docs/agents/](../docs/agents/) | Reference documentation |

OpenCode reads `AGENTS.md` natively and uses `opencode.jsonc` as a harness overlay.

## See Also

- [Harness mapping](../docs/agents/harness-mapping.md)
- [Open Agent Format](https://openagentformat.com/)
- [OpenCode config schema](https://opencode.ai/config.json)
