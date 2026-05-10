# Zsh initialization for interactive shells

# Export 'SHELL' to child processes. Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

[[ -o interactive ]] || return

# Load modular zsh configuration
for config in "${ZDOTDIR:-${HOME}}/.zsh.d"/*.zsh(N); do
    [[ -r "$config" ]] && source "$config"
done
unset config
