# Zsh login shell initialization

if [[ -z "${DOTFILES_ZSH_ENV_LOADED:-}" ]]; then
    typeset -g DOTFILES_ZSH_ENV_LOADED=1

    # Source POSIX profile first (XDG variables, basic PATH).
    if [[ -f "${HOME}/.profile" ]]; then
        emulate sh -c '. "${HOME}/.profile"'
    fi

    if [[ -x /opt/homebrew/bin/brew ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv zsh)"
    elif [[ -x /usr/local/bin/brew ]]; then
        eval "$(/usr/local/bin/brew shellenv zsh)"
    fi

    if [[ -d "${HOME}/.tfenv/bin" ]]; then
        path=("${HOME}/.tfenv/bin" $path)
    fi

    typeset -U path PATH
fi
