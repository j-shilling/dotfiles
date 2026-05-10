# Zsh login shell initialization

# Source POSIX profile first (XDG variables, basic PATH)
if [[ -f "${HOME}/.profile" ]]; then
    emulate sh -c '. "${HOME}/.profile"'
fi

export HISTFILE="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh/history"
