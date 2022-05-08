export SHELL

if [[ $- != *i* ]] ; then
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile
    return
fi

source /etc/bashrc

export GUIX_PROFILE="$HOME/.guix-profile/"
if [ -e "$GUIX_PROFILE" ] ; then
    . "$GUIX_PROFILE/etc/profile"
fi

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

alias ls='ls -p --color=auto'
alias grep='grep --color=auto'
