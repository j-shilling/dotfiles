# Set up the system, user profile, and related variables.
# /etc/profile will be sourced by bash automatically
# Set up the home environment profile.
if [ -f ~/.profile ]; then source ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

export HISTFILE="$XDG_CACHE_HOME/.bash_history"
export HISTFILESIZE="100000"
export HISTIGNORE="ls:exit:history:clear"
