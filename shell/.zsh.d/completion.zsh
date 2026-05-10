# Zsh completion configuration

autoload -Uz compinit

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose true
zstyle ':completion:*:descriptions' format '%F{yellow}%d%f'

zmodload zsh/complist

local zcompdump="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh/.zcompdump"
mkdir -p "${zcompdump:h}"
compinit -d "$zcompdump"
