---
name: "gnu-stow"
description: >
  Invoke GNU Stow to manage symlink farms — stow, restow, unstow, and simulate
  package deployment. Use when running stow commands, deploying dotfiles via
  symlinks, fixing stow conflicts, understanding -d/-t flags, restow after file
  changes, unstow with -D, dry-run with -n, or .stow-local-ignore rules — even
  if the user only says "symlink config" or "install package to home" without
  naming Stow.
license: "MIT"
metadata:
  author: "j-shilling"
  version: "0.1.0"
allowed-tools: ["bash", "read"]
---

# GNU Stow CLI

GNU Stow creates symlinks from a **stow directory** (package trees) into a **target directory** (e.g. `$HOME`). It is not git — it does not track history; it only manages symlinks.

## Terminology

| Term | Flag | Meaning |
|------|------|---------|
| Stow directory | `-d DIR` | Root containing package subdirectories |
| Target directory | `-t DIR` | Where symlinks are created |
| Package | (positional) | Top-level subdirectory name under stow dir |

**Always pass `-d` and `-t` explicitly.** Defaults (cwd = stow dir, target = parent of stow dir) are error-prone.

## Command template

```bash
stow -d <stow-dir> -t <target-dir> [options] [action] <package>...
```

Example:

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -R emacs
```

## Choose an action

| Goal | Flag | Command fragment |
|------|------|------------------|
| First install | `-S` (default) | `stow -d DIR -t TARGET -S pkg` |
| Update after package edits | `-R` | `stow -d DIR -t TARGET -R pkg` |
| Remove symlinks from target | `-D` | `stow -d DIR -t TARGET -D pkg` |
| Preview without changes | `-n` | `stow -n -d DIR -t TARGET -R pkg` |

**Prefer `-R` (restow)** after adding, removing, or renaming files inside a package. Restow unstows then stows again, pruning obsolete symlinks.

## Recipes

### Stow one package (first install)

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v emacs
```

### Restow after editing package files

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -R emacs
```

### Unstow (remove symlinks from target)

```bash
stow -d ~/dotfiles -t "${HOME}" --no-folding -v -D emacs
```

Unstow only removes symlinks **Stow owns**. It never deletes files inside the package directory.

### Dry-run (simulate)

```bash
stow -n -d ~/dotfiles -t "${HOME}" --no-folding -v -R emacs
```

Use `-n` before `-D` or when diagnosing conflicts.

### Verify symlinks

```bash
readlink ~/.config/emacs/init.el
ls -la ~/.config/emacs/init.el
```

Expected: symlink pointing into the stow directory (relative path into package tree).

## Flags you must know

### `--no-folding`

Disable tree folding. Without it, Stow may symlink an entire subtree (e.g. `~/.config` → one package's `.config`). **Use `--no-folding` when multiple packages share parent directories** (typical dotfiles layout: `emacs`, `git`, `agents` all under `~/.config/`).

### `-v` / `--verbose`

Print operations to stderr. Levels 0–5; `-v` increments by one, `--verbose=N` sets level.

### Do not use casually

- **`--adopt`**: moves existing real files into the package tree. Destructive to layout; only when user explicitly requests.
- **Default paths**: never omit `-d`/`-t` unless you have verified cwd and target.

## Gotchas

1. **Not git** — stow manages symlinks only; version control is separate.
2. **Never deletes package trees** — unstow removes target symlinks, not source files in the stow directory.
3. **Conflicts abort cleanly** — since Stow 2.0, conflicts are detected before any changes; the filesystem is left untouched on failure.
4. **Conflict causes** — a real file or non-Stow directory already exists where Stow needs a symlink.
5. **Ignore lists** — `.stow-local-ignore` in each package root excludes paths from stowing (Perl regexes, one per line).
6. **`--dotfiles`** — maps `dot-` prefix to `.` (e.g. `dot-bashrc` → `.bashrc`). Only needed when packages use that naming convention.

## Deep reference

For full option tables, tree folding, ignore-list matching rules, and conflict details, read [`references/stow-cli.md`](references/stow-cli.md).

Official docs: `man stow`, `info stow`, [GNU Stow manual](https://www.gnu.org/software/stow/manual/stow.html).
