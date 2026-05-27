# Development tools and integrations

# Emacs integration
if [[ -n "$EAT_SHELL_INTEGRATION_DIR" && -r "${EAT_SHELL_INTEGRATION_DIR}/zsh" ]]; then
    source "${EAT_SHELL_INTEGRATION_DIR}/zsh"
fi

# Add Emacs bin to PATH if it exists
if [[ -d "${XDG_CONFIG_HOME}/emacs/bin" ]]; then
    path+=("${XDG_CONFIG_HOME}/emacs/bin")
fi

# Emacs VTerm integration
if [[ "$INSIDE_EMACS" == 'vterm' ]] \
    && [[ -n "$EMACS_VTERM_PATH" ]] \
    && [[ -f "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh" ]]; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
fi

# LSP Mode
export LSP_USE_PLISTS=true

# FZF shell integration
if [[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ]]; then
    source /usr/share/doc/fzf/examples/key-bindings.zsh 2>/dev/null
elif [[ -f /opt/homebrew/opt/fzf/shell/key-bindings.zsh ]]; then
    source /opt/homebrew/opt/fzf/shell/key-bindings.zsh 2>/dev/null
fi

if [[ -f /usr/share/doc/fzf/examples/completion.zsh ]]; then
    source /usr/share/doc/fzf/examples/completion.zsh 2>/dev/null
elif [[ -f /opt/homebrew/opt/fzf/shell/completion.zsh ]]; then
    source /opt/homebrew/opt/fzf/shell/completion.zsh 2>/dev/null
fi

# direnv integration
command -v direnv >/dev/null 2>&1 && eval "$(direnv hook zsh)"

# Keep the local CA bundle current after Homebrew changes.
if command -v brew >/dev/null 2>&1; then
    brew() {
        command brew "$@"
        local status=$?

        case "$1" in
            install|upgrade|reinstall|update|bundle)
                if [ "$status" -eq 0 ] && command -v update-ca-bundle >/dev/null 2>&1; then
                    update-ca-bundle >/dev/null 2>&1 \
                        || printf 'update-ca-bundle failed; run it manually for details\n' >&2
                fi
                ;;
        esac

        return "$status"
    }
fi

typeset -U path PATH
