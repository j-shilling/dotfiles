export SHELL

if [[ $- != *i* ]] ; then
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile
    return
fi

source /etc/bash.bashrc
source $HOME/.common_profile

PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "

set -o emacs

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

eval "$(direnv hook bash)"
