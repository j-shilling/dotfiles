# Global Codex Guidance

This file is stowed from `~/dotfiles/agents/.codex/AGENTS.md`. It is the only
file this dotfiles package intentionally places under `~/.codex`.

## Configuration Boundary

- Keep `~/.codex/config.toml`, profile files such as
  `~/.codex/work.config.toml`, authentication, sessions, plugin cache,
  memories, logs, project trust, and app state local to this computer.
- Do not copy machine-local or work-account settings from `~/.codex` into the
  dotfiles repository unless the user explicitly asks for that specific value.
- Use Codex-supported config files and profiles for runtime settings; use this
  file only for durable global instructions.

## Shared Agent Assets

- When reusable agent assets are relevant, check `$HOME/.agents` if it exists.
- Prefer `$HOME/.agents/skills/<name>/SKILL.md` for user-level reusable skills.
  Codex discovers these skills natively, and repository-specific skills may
  also live under `.agents/skills` inside a project.
- Use `$HOME/.agents/plugins/marketplace.json` for the user's personal plugin
  marketplace when present.
- If the user asks about reusable agents or subagents, inspect
  `$HOME/.agents/agents` and `$HOME/.agents/subagents` when present, but adapt
  the result to the Codex-supported custom-agent surface if needed.
- Project `AGENTS.md` files are still authoritative for repository-specific
  commands, conventions, tests, and review expectations.
