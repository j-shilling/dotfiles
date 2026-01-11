# Claude Code Configuration

This stow package manages Claude Code configuration files that should be shared across machines.

## Contents

- **`.claude/settings.json`** - User-level Claude Code settings
- **`.claude/settings.local.json`** - Local overrides for permissions and machine-specific settings
- **`.claude/CLAUDE.md`** - Personal user-level context provided to Claude across all projects
- **`.claude/agents/`** - Custom subagents available across all projects
- **`.claude/hooks/`** - User-level hook scripts

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

These files are managed by Claude Code and should not be version controlled.

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

## Adding Custom Subagents

Add custom subagent markdown files to `.claude/agents/`:

```bash
# Create a new subagent
vim ~/dotfiles/claude/.claude/agents/my-agent.md

# Restow to update symlinks
stow -R -d ~/dotfiles/ -t "${HOME}" --no-folding -v claude
```

## See Also

- [Claude Code Documentation](https://docs.claude.ai/claude-code)
- Main dotfiles: `CLAUDE.md` in repository root
