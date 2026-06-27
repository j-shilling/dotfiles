# GNU Stow CLI Reference

Distilled from `man stow`, `info stow`, and the [GNU Stow manual](https://www.gnu.org/software/stow/manual/stow.html) (v2.3–2.4).

## Syntax

```
stow [OPTIONS] [ACTION FLAG] PACKAGE ...
```

Each `PACKAGE` is a subdirectory name inside the stow directory.

## Terminology

- **Stow directory**: root containing one subdirectory per package (e.g. `~/dotfiles`)
- **Target directory**: root where symlinks appear (e.g. `$HOME`, `/usr/local`)
- **Package directory**: `stow-dir/package-name/`
- **Installation image**: file layout inside a package, mirroring paths relative to the target
- **Tree folding**: Stow may symlink an entire subtree with one link instead of creating many leaf symlinks
- **Ownership**: Stow "owns" symlinks in the target that point into its packages; it only removes what it owns

## Environment

| Variable | Effect |
|----------|--------|
| `STOW_DIR` | Default stow directory if `-d` omitted |

Prefer explicit `-d` and `-t` over defaults.

## Options

| Option | Description |
|--------|-------------|
| `-d DIR`, `--dir=DIR` | Stow directory (default: cwd or `STOW_DIR`) |
| `-t DIR`, `--target=DIR` | Target directory (default: parent of stow dir) |
| `-v`, `--verbose[=N]` | Verbose output to stderr; levels 0–5 |
| `-n`, `--no` | Simulate only; no filesystem changes |
| `-S`, `--stow` | Install packages (default action) |
| `-D`, `--delete` | Unstow — remove symlinks Stow owns from target |
| `-R`, `--restow` | Unstow then stow; prunes obsolete symlinks |
| `--no-folding` | Disable tree folding and refolding |
| `--ignore=REGEX` | Ignore files matching Perl regex (repeatable) |
| `--defer=REGEX` | Skip stowing if file already stowed by another package |
| `--override=REGEX` | Force stow even if another package owns the file |
| `--dotfiles` | Map `dot-` prefix to `.` in package filenames |
| `--adopt` | Move conflicting real files into package tree (alters stow dir) |
| `-V`, `--version` | Print version |
| `-h`, `--help` | Print help |

## Actions in detail

### Stow (`-S`, default)

Creates symlinks in the target tree pointing into the package tree. Stow minimizes symlink count via tree folding unless `--no-folding` is set.

### Restow (`-R`)

Equivalent to unstow followed by stow for the named packages. Use after:

- Adding files to a package
- Removing files from a package
- Renaming paths inside a package

### Unstow (`-D`)

Recursively scans the target tree, removes symlinks pointing into the package being deleted, removes empty directories that held only those symlinks, and may refold previously split trees.

Does **not**:

- Delete files inside the stow or package directories
- Remove real (non-symlink) files in the target
- Remove symlinks it does not own

### Simulate (`-n`)

Shows what would happen without modifying the filesystem. Combine with `-D` or `-R` to preview destructive operations.

## Tree folding

Without `--no-folding`, if Stow can link an entire directory with one symlink, it will. Example: first package to install may make `/usr/local/bin` → `stow/perl/bin`.

When a second package needs files under an already-folded directory, Stow **splits open** the tree: removes the folding symlink, creates a real directory, and populates it with leaf symlinks.

**Dotfiles implication**: multiple packages under `~/.config/` require `--no-folding` so each package symlinks individual files rather than claiming the whole `.config` directory.

## Ignore lists

Three levels (first match wins for package-local):

1. `.stow-local-ignore` in the package directory
2. `~/.stow-global-ignore`
3. Built-in default (`.git`, `CVS`, `*.~`, `#*#`, etc.)

Format: Perl regular expressions, one per line. `#` starts comments. Escape `#` in patterns with `\#`.

### Matching algorithm

Stow computes the path relative to the package root, prefixed with `/` (e.g. `/foo/bar/bazqux`).

1. Patterns **containing `/`**: must exactly match a subpath of the relative path
2. Patterns **without `/`**: must exactly match the basename
3. Otherwise the file is stowed

`.stow-local-ignore` itself is always ignored.

### Example patterns

```
\.git
.*\.org
^/README.*
\.#.+          # emacs lock files
!prompts/.*\.md   # negation not in basic stow — use careful patterns instead
```

Note: Stow ignore lists do not support gitignore-style negation (`!`). Use explicit patterns.

## Conflicts

A conflict occurs when Stow cannot create a needed symlink because:

- A real file exists at the target path (not owned by Stow)
- A directory exists where a symlink to a non-directory is needed
- A folded tree cannot be split as required

**Not a conflict**: an existing symlink already pointing to the correct package location.

### Two-phase execution (Stow ≥ 2.0)

Stow scans for all potential conflicts before performing any stow/unstow operations. If conflicts exist, it prints them and **exits without modifying the filesystem**.

### Resolving conflicts

1. Run with `-n` to diagnose
2. Identify the blocking real file in the target tree
3. Options: move/remove the conflicting file, use `--adopt` (only if appropriate), or restructure packages

## Mixing operations

Multiple actions in one invocation:

```bash
stow -d ~/dotfiles -t ~ -S pkg1 pkg2 -D pkg3 -R pkg4
```

Stow merges operations before executing to minimize inconsistency windows.

## `--dotfiles` mode

When enabled, files named `dot-*` in the package are stowed with the `dot-` prefix replaced by `.`:

- `dot-bashrc` → `~/.bashrc`
- `dot-emacs.d/init.el` → `~/.emacs.d/init.el`

Files not prefixed with `dot-` are processed normally. Not needed when packages contain literal dotfiles (e.g. `shell/.bashrc`).

## `--adopt` warning

When stowing, if a target path exists as a plain file not owned by any package, `--adopt` moves it into the package's installation image within the stow directory, then proceeds. This **modifies the stow directory contents**. Do not use unless the user explicitly wants to claim existing files.

## Resource files

- `~/.stowrc` — default CLI options (shifted onto argv at runtime)
- Per-package `.stowrc` — options when run from that package context

## Further reading

- `man stow`
- `info stow` (more complete than man page)
- https://www.gnu.org/software/stow/manual/stow.html
