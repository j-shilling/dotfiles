# Bash login shell initialization
# Sourced by bash for login shells

# Source POSIX profile first (XDG variables, basic PATH)
if [ -f ~/.profile ]; then
    source ~/.profile
fi

# Source bashrc for interactive shells (already sourced by .profile for interactive shells)
# This is here as a fallback if .profile doesn't source it
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi
export PATH="$HOME/.tfenv/bin:$PATH"
