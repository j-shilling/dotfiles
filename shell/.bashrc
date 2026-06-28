# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]; then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Machine-specific secrets for non-login interactive bash.
if [ -z "${DOTFILES_SECRETS_LOADED:-}" ]; then
    DOTFILES_SECRETS_FILE="${XDG_CONFIG_HOME:-${HOME}/.config}/shell/secrets.env"
    if [ -r "${DOTFILES_SECRETS_FILE}" ]; then
        DOTFILES_SECRETS_LOADED=1
        case $- in
            *a*) . "${DOTFILES_SECRETS_FILE}" ;;
            *) set -a; . "${DOTFILES_SECRETS_FILE}"; set +a ;;
        esac
    fi
    unset DOTFILES_SECRETS_FILE
fi

# Source the system-wide file.
[ -f /etc/bashrc ] && source /etc/bashrc

# Add ~/.local/bin to PATH early (before other tools)
export PATH="${HOME}/.local/bin:${PATH}"

# Load modular bash configuration
for config in "${HOME}/.bash.d"/*.bash; do
    [ -r "$config" ] && source "$config"
done
unset config

# opencode
for _opencode_bin in "$HOME/.opencode/bin" "$HOME/.local/bin/opencode"; do
    [ -d "$_opencode_bin" ] && export PATH="$_opencode_bin:$PATH" && break
done
unset _opencode_bin
