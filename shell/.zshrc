# -*- mode: sh; -*-
# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="candy"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

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

source $HOME/dotfiles/shell/.common_profile
