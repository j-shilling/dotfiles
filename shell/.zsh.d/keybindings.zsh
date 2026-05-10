# Zsh line editing and key bindings

bindkey -e

# Match the readline history prefix search bindings from ~/.inputrc.
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic
