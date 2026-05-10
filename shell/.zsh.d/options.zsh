# Zsh shell options

setopt append_history          # Append to history file, don't overwrite
setopt extended_history        # Save timestamps and command duration
setopt hist_ignore_dups        # Ignore immediately repeated commands
setopt hist_reduce_blanks      # Remove extra blanks from history entries
setopt inc_append_history      # Write commands to history immediately
setopt share_history           # Share history across zsh sessions
setopt auto_cd                 # Auto-cd to directory if command is a directory name
setopt cdable_vars             # Try variable expansion on cd argument
setopt correct                 # Autocorrect minor spelling errors
setopt extended_glob           # Enable extended glob operators
setopt glob_star_short         # Enable ** for recursive globbing
setopt no_beep                 # Disable terminal bell
setopt prompt_subst            # Allow parameter expansion in prompts

unsetopt nomatch               # Leave unmatched globs unchanged

HISTFILE="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh/history"
HISTSIZE=100000
SAVEHIST=100000

mkdir -p "${HISTFILE:h}"
