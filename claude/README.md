# Claude Code Configuration

This stow package manages Claude Code configuration files that should be shared across machines.

## Contents

- **`.claude/settings.json`** - User-level Claude Code settings
- **`.claude/settings.local.json`** - Local overrides for permissions and machine-specific settings
- **`.claude/CLAUDE.md`** - Personal user-level context provided to Claude across all projects
- **`.claude/agents/`** - Custom subagents available across all projects (OAF wrappers + personal agents)
- **`.claude/hooks/`** - User-level hook scripts (optional)

## Installation

Install with stow from the dotfiles directory:

```bash
stow -d ~/dotfiles/ -t "${HOME}" --no-folding -v claude
```

## What's NOT Included

The following ephemeral/sensitive files are excluded via `.stow-local-ignore`:

- `~/.claude.json` - Contains OAuth tokens, session data, and MCP server state
- Cache directories and history files
- Session snapshots and debugging data
- Plugin marketplace data
- `.credentials.json`

These files are managed by Claude Code and should not be version controlled.

## Plugins

The `enabledPlugins` section in `settings.json` contains all installed plugins. When you stow this package on a new machine, Claude Code will prompt you to install these plugins automatically.

Currently enabled plugins:

**claude-plugins-official:** frontend-design, context7, github, feature-dev, code-review, commit-commands, typescript-lsp, security-guidance, playwright, agent-sdk-dev, figma

**anthropic-agent-skills:** document-skills, example-skills

**claude-code-workflows:** accessibility-compliance, agent-orchestration, api-testing-observability, backend-api-security, backend-development, cicd-automation, cloud-infrastructure, code-documentation, context-management, database-design, database-migrations, deployment-strategies, developer-essentials, documentation-generation, full-stack-orchestration, functional-programming, javascript-typescript, observability-monitoring, security-compliance, security-scanning, tdd-workflows, team-collaboration

**fallow-skills:** fallow

**grafana-skills:** grafana-plugins, grafana-cloud, grafana-core

To install plugins, run `/plugin` in Claude Code. The configuration will be automatically updated.

## Subagents

OAF sub-agents from `subagents/` have thin Claude wrappers in `.claude/agents/`:

- `emacs-maintainer.md` — delegates to `subagents/emacs-maintainer/AGENTS.md`
- `config-reviewer.md` — delegates to `subagents/config-reviewer/AGENTS.md`

Personal agents (e.g. `effect-*-writer.md`) may exist only on specific machines under `~/.claude/agents/` and are not stowed.

## Configuration Hierarchy

Claude Code uses the following precedence (highest to lowest):

1. Managed settings (system-level, enterprise deployments)
2. Command-line arguments
3. Local settings (`.claude/settings.local.json`)
4. Project settings (`.claude/settings.json` in repository)
5. User settings (`~/.claude/settings.json` - this stow package)

## MCP Servers

MCP server configuration should be stored in:

- **User-level**: `~/.claude.json` (not in stow, contains credentials)
- **Project-level**: `.mcp.json` in each repository (checked into git)

See [ai-integration.md](../docs/agents/ai-integration.md) for the cross-harness MCP inventory table.

## Adding Custom Subagents

Add custom subagent markdown files to `.claude/agents/`:

```bash
# Create a new subagent
vim ~/dotfiles/claude/.claude/agents/my-agent.md

# Restow to update symlinks
stow -R -d ~/dotfiles/ -t "${HOME}" --no-folding -v claude
```

## Project context (dotfiles repo)

When working in the dotfiles repository itself:

- **[AGENTS.md](../AGENTS.md)** — canonical OAF manifest (Open Agent Format)
- **[CLAUDE.md](../CLAUDE.md)** — thin bridge that imports `@AGENTS.md` for Claude Code
- **[docs/agents/](../docs/agents/)** — progressive disclosure reference docs

User-level preferences in `.claude/CLAUDE.md` (this package) apply across all projects and are separate from project context.

## See Also

- [Claude Code Documentation](https://docs.claude.ai/claude-code)
- [Open Agent Format](https://openagentformat.com/)
- [Harness mapping](../docs/agents/harness-mapping.md)
