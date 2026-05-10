# Zsh aliases for interactive shells

if ls --color=auto >/dev/null 2>&1; then
    alias ls='ls -p --color=auto'
else
    alias ls='ls -p -G'
fi

alias ll='ls -l'

if grep --color=auto '' /dev/null >/dev/null 2>&1; then
    alias grep='grep --color=auto'
fi

if command -v ip >/dev/null 2>&1; then
    alias ip='ip -color=auto'
fi
