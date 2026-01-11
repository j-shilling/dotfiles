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
