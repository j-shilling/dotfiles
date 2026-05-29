# Zsh login shell initialization

# Source POSIX profile first (XDG variables, basic PATH)
if [[ -f "${HOME}/.profile" ]]; then
    emulate sh -c '. "${HOME}/.profile"'
fi

eval "$(/opt/homebrew/bin/brew shellenv zsh)"
export PATH="$HOME/.tfenv/bin:$PATH"
