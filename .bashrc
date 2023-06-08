export SHELL

if [[ $- != *i* ]] ; then
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile
    return
fi

if [ -e /etc/bash.bashrc ] ; then
    source /etc/bash.bashrc
fi

if [ -e "${HOME}/.common_profile" ] ; then
    source $HOME/.common_profile
fi

if [[ "${INSIDE_EMACS}" = 'vterm' ]] ; then
  if [[ -n "${EMACS_VTERM_PATH}" ]] && [[ -f "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh" ]] ; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
  fi
fi

PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "

if ((BASH_VERSINFO[0] >= 4)) ; then
    PROMPT_DIRTRIM=2
fi


PROMPT_COMMAND='history -a'

HISTSIZE=500000
HISTFILESIZE=100000
HISTCONTROL="erasedups:ignoreboth"
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"


bind Space:magic-space

bind "set completion-ignore-case on"
bind "set completion-map-case on"
bind "set show-all-if-ambiguous on"
bind "set mark-symlinked-directories on"

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

set -o emacs
set -o noclobber
set -o history

shopt -s histappend
shopt -s cmdhist
shopt -s checkwinsize
shopt -s autocd
shopt -s dirspell
shopt -s cdspell
shopt -s globstar
shopt -s nocaseglob;

eval "$(direnv hook bash)"
. /home/jake/.asdf/asdf.sh
PROG=sg source /home/jake/.sourcegraph/sg.bash_autocomplete
