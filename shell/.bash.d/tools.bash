# Development tools and integrations

# Emacs integration
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "${EAT_SHELL_INTEGRATION_DIR}/bash"

# Add Emacs bin to PATH if it exists
if [[ -d "${XDG_CONFIG_HOME}/emacs/bin" ]]; then
    export PATH="${PATH}:${XDG_CONFIG_HOME}/emacs/bin"
fi

# Emacs VTerm integration
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
fi

# LSP Mode
export LSP_USE_PLISTS=true

# FZF shell integration
[ -f /usr/share/doc/fzf/examples/key-bindings.bash ] && source /usr/share/doc/fzf/examples/key-bindings.bash
[ -f /usr/share/bash-completion/completions/fzf ] && source /usr/share/bash-completion/completions/fzf

# direnv integration
command -v direnv >/dev/null && eval "$(direnv hook bash)"

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
