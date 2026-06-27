---
name: "unix-manuals"
description: >
  Consult local Unix manual pages with man and info before guessing CLI flags.
  Use when configuring niche command-line tools, looking up man pages, Texinfo
  docs, apropos searches, verifying installed-version behavior, or when the
  user says "check the manual" — even for stow, mbsync, msmtp, notmuch, pass,
  or other poorly represented tools not named explicitly.
license: "MIT"
metadata:
  author: "j-shilling"
  version: "0.1.0"
allowed-tools: ["bash", "read"]
---

# Unix Manuals (man and info)

Consult **local** documentation on the machine before inventing flags. Training data is often wrong or outdated for niche tools.

## When to consult

**Do consult** for:

- Niche CLIs poorly represented in training data (GNU Stow, isync/mbsync, msmtp, notmuch, pass)
- Flag syntax when configuring dotfiles for those tools
- Verifying behavior matches the **installed version**

**Skip** unless version-specific:

- Git basics, common shell builtins, widely documented tools
- Library/framework APIs — use Context7 or web search instead

## man workflow

Check whether a page exists, then read non-interactively:

```bash
MANPAGER=cat man -w <name>                    # path to man page, or error
MANPAGER=cat man <name> 2>/dev/null | col -b | head -200
man -k <keyword>                              # apropos search by keyword
man <section> <name>                          # e.g. man 5 crontab
```

Common sections: `1` user commands, `5` file formats, `8` admin.

## info workflow (GNU Texinfo)

GNU tools often have **more complete** docs in Texinfo than man. If man says "see info", use info.

```bash
info -w <name> 2>/dev/null                    # locate info file
info <name> 2>/dev/null | col -b | head -200
info --subnodes -f /usr/share/info/<file> 2>/dev/null | col -b | head -300
```

Example: Stow's man page defers to `info stow` for definitive documentation.

## Decision table

| Situation | Prefer |
|-----------|--------|
| GNU tool; man says "see info" | `info` first |
| Quick flag check; man page exists | `man` |
| Don't know the command name | `man -k <keyword>` or `apropos <keyword>` |
| Library/framework API (npm, PyPI, etc.) | Context7 / web, not man |
| Tool not installed | `which <name>`; then web if missing |

## Agent execution notes

1. **Non-interactive only** — set `MANPAGER=cat` and `PAGER=cat`; pipe through `col -b` to strip backspace formatting; use `head` on large pages.
2. **Sandbox failures** — if `man`/`info` fails unexpectedly, retry with full shell permissions.
3. **Cite sources** — when advising flags, note they came from man/info (section or node name).
4. **Distill repeated lookups** — if the same tool is consulted often in this repo, suggest adding `skills/<tool>/references/` (see `skills/gnu-stow/references/stow-cli.md`).

## Relationship to other skills

| Layer | Skill | Role |
|-------|-------|------|
| Meta | `unix-manuals` (this) | How to read man/info live |
| Tool | e.g. `gnu-stow` | Distilled procedures + `references/` |
| Repo | e.g. `stow-dotfiles` | This dotfiles repo's conventions |

If a tool skill exists, read it first; use this skill when you need version-specific detail or no tool skill exists yet.
