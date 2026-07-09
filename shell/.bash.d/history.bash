# Bash history settings

export HISTFILE="${XDG_CACHE_HOME}/.bash_history"
export HISTSIZE=100000
export HISTFILESIZE=100000
export HISTCONTROL=ignoredups:erasedups
export HISTIGNORE="ls:exit:history:clear"
export HISTTIMEFORMAT="%F %T "

# Append to history file immediately, not just on shell exit.
# This prevents history loss between concurrent terminal sessions.
PROMPT_COMMAND="history -a;${PROMPT_COMMAND:+$PROMPT_COMMAND}"