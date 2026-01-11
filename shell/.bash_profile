# Bash login shell initialization
# Sourced by bash for login shells

# Source POSIX profile first (XDG variables, basic PATH)
if [ -f ~/.profile ]; then
    source ~/.profile
fi

# Bash-specific login configuration
export HISTFILE="${XDG_CACHE_HOME}/.bash_history"
export HISTFILESIZE="100000"
export HISTIGNORE="ls:exit:history:clear"

# Source bashrc for interactive shells (already sourced by .profile for interactive shells)
# This is here as a fallback if .profile doesn't source it
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi
