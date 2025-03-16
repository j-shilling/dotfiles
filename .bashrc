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

# Sourcing
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"

# PyEnv
export PYENV_ROOT="${HOME}/.pyenv"
[[ -d "${PYENV_ROOT}/bin" ]] && export PATH="${PYENV_ROOT}/bin:${PATH}"
eval "$(pyenv init - bash)"

# FZF Shell Integration
#eval $(fzf --bash)
