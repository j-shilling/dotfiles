# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
[ -f /etc/bashrc ] && source /etc/bashrc

alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
alias ip='ip -color=auto'

PS1='\u@\h \w\$ '

# Shell Options
shopt -s histappend
shopt -s cmdhist
shopt -s checkwinsize
shopt -s autocd
shopt -s cdable_vars
shopt -s dirspell
shopt -s cdspell
shopt -s globstar
shopt -s nocaseglob
shopt -s checkhash
shopt -s lithist

export GPG_TTY="$(tty)"

# Sourcing
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "${EAT_SHELL_INTEGRATION_DIR}/bash"

# Path
export PATH="${HOME}/.local/bin:${PATH}"
[ -d "${XDG_CONFIG_HOME}/emacs/bin" ] && export PATH="${PATH}:${XDG_CONFIG_HOME}/emacs/bin"

# PyEnv
export PYENV_ROOT="${HOME}/.pyenv"
[[ -d "${PYENV_ROOT}/bin" ]] && export PATH="${PYENV_ROOT}/bin:${PATH}"
eval "$(pyenv init - bash)"

# NVM
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# FZF Shell Integration
[ -f /usr/share/doc/fzf/examples/key-bindings.bash ] && source /usr/share/doc/fzf/examples/key-bindings.bash
[ -f /usr/share/bash-completion/completions/fzf ] && source /usr/share/bash-completion/completions/fzf

# PHP
export PATH="/home/jake/.config/herd-lite/bin:$PATH"
export PHP_INI_SCAN_DIR="/home/jake/.config/herd-lite/bin:$PHP_INI_SCAN_DIR"

# direnv
eval "$(direnv hook bash)"

# VTerm
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi

# LSP Mode
export LSP_USE_PLISTS=true
