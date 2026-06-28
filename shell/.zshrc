# Zsh initialization for interactive shells

# Export 'SHELL' to child processes. Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

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

[[ -o interactive ]] || return

# Load modular zsh configuration
for config in "${ZDOTDIR:-${HOME}}/.zsh.d"/*.zsh(N); do
    [[ -r "$config" ]] && source "$config"
done
unset config

# opencode
for _opencode_bin in "$HOME/.opencode/bin" "$HOME/.local/bin/opencode"; do
    [[ -d "$_opencode_bin" ]] && export PATH="$_opencode_bin:$PATH" && break
done
unset _opencode_bin
