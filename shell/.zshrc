# -*- mode: sh; -*-

source $HOME/dotfiles/shell/.common_profile

export ZSH="${HOME}/.oh-my-zsh"

ZSH_THEME="candy"

HYPHEN_INSENSITIVE="true"

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

ZSH_COMPLETIONS_DIR="${ZSH_CUSTOM:=${HOME}/.oh-my-zsh/custom}/plugins/zsh-completions"
if [ ! -d ${ZSH_COMPLETIONS_DIR} ] ; then
  git clone https://github.com/zsh-users/zsh-completions ${ZSH_COMPLETIONS_DIR}
fi

FZF_TAB_COMPLETION_DIR="${ZSH_CUSTOM:=${HOME}/.oh-my-zsh/custom}/plugins/fzf-tab-completion"
if [ ! -d ${FZF_TAB_COMPLETION_DIR} ] ; then
  git clone https://github.com/lincheney/fzf-tab-completion.git ${FZF_TAB_COMPLETION_DIR}
fi

plugins=(
  ag
  aliases
  alias-finder
  aws
  colored-man-pages
  colorize
  command-not-found
  common-aliases
  copypath
  copyfile
  cp
  direnv
  docker
  docker-compose
  dotnet
  emacs
  fd
  fzf
  git
  git-auto-fetch
  git-extras
  git-prompt
  history
  kubectl
  lein
  man
  minikube
  mvn
  ng
  node
  npm
  nvm
  ripgrep
  safe-paste
  shrink-path
  spring
  sudo
  systemadmin
  systemd
  vim-interaction
  vi-mode
  tmux
  tmux-cssh
  zsh-interactive-cd
  zsh-completions
)

mac_plugins=(
  brew
  gnu-utils
  iterm2
  osx
)

ZSH_TMUX_AUTOSTART=false
ZSH_TMUX_AUTOSTART_ONCE=false
ZSH_TMUX_AUTOCONNECT=false
ZSH_TMUX_AUTOQUIT=false
ZSH_TMUX_FIXTERM=true
ZSH_TMUX_UNICODE=true

if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

export DISABLE_FZF_AUTO_COMPLETION="false"
export DISABLE_FZF_KEY_BINDINGS="false"

if [ -e $ZSH/oh-my-zsh.sh ] ; then
  source $ZSH/oh-my-zsh.sh
fi

if [ -e $FZF_TAB_COMPLETION_DIR/zsh/fzf-zsh-completion.sh ] ; then
  source $FZF_TAB_COMPLETION_DIR/zsh/fzf-zsh-completion.sh
fi
