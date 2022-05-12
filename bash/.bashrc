export SHELL

if [[ $- != *i* ]] ; then
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile
    return
fi

source /etc/bashrc

if [ -d "$GUIX_PROFILE/etc/bash_completion.d" ] ; then
    for f in $GUIX_PROFILE/etc/bash_completion.d/* ; do
        source $f
    done
fi

PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "

set -o vi

set -o noclobber

shopt -s checkwinsize

if ((BASH_VERSINFO[0] >= 4)) ; then
    PROMPT_DIRTRIM=2
fi

bind Space:magic-space
shopt -s globstar 2> /dev/null

shopt -s nocaseglob;

bind "set completion-ignore-case on"

bind "set completion-map-case on"

bind "set show-all-if-ambiguous on"

bind "set mark-symlinked-directories on"

shopt -s histappend

shopt -s cmdhist

PROMPT_COMMAND='history -a'

HISTSIZE=500000
HISTFILESIZE=100000
HISTCONTROL="erasedups:ignoreboth"
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

shopt -s autocd 2> /dev/null
shopt -s dirspell 2> /dev/null
shopt -s cdspell 2> /dev/null

# Build Path Based on Which Dirs Exist
if [ -d "${HOME}/.local/bin" ] ; then
  export PATH="${HOME}/.local/bin:${PATH}"
fi

if [ -d "${HOME}/.emacs.d/bin" ] ; then
  export PATH="${HOME}/.emacs.d/bin:${PATH}"
fi

if [ -d "${HOME}/.cask/bin" ] ; then
  export PATH="${HOME}/.cask/bin:${PATH}"
fi

if [ -d "${HOME}/.eldev/bin" ] ; then
  export PATH="${HOME}/.eldev/bin:${PATH}"
fi

if [ -d "/var/lib/snapd/snap/bin" ] ; then
  export PATH="/var/lib/snapd/snap/bin:${PATH}"
fi

if [ -d "${HOME}/.dotnet/tools" ] ; then
  export PATH="${HOME}/.dotnet/tools:${PATH}"
fi

if [ -d "/opt/piavpn/bin" ] ; then
  export PATH="/opt/piavpn/bin:${PATH}"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias ls='ls -p --color=auto'
alias grep='grep --color=auto'
