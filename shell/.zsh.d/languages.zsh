# Language runtime managers and toolchains

# Python (PyEnv)
export PYENV_ROOT="${HOME}/.pyenv"
if [[ -d "${PYENV_ROOT}/bin" ]]; then
    path=("${PYENV_ROOT}/bin" $path)
fi
command -v pyenv >/dev/null 2>&1 && eval "$(pyenv init - zsh)"

# Node.js (NVM)
export NVM_DIR="${XDG_CONFIG_HOME:-${HOME}/.config}/nvm"
if [[ -s "$NVM_DIR/nvm.sh" ]]; then
    source "$NVM_DIR/nvm.sh"
elif [[ -s "/opt/homebrew/opt/nvm/nvm.sh" ]]; then
    export NVM_DIR="${HOME}/.nvm"
    source "/opt/homebrew/opt/nvm/nvm.sh"
fi

# Node.js (pnpm)
export PNPM_HOME="${HOME}/.local/share/pnpm"
if [[ ":$PATH:" != *":$PNPM_HOME:"* ]]; then
    path=("$PNPM_HOME" $path)
fi

# PHP (Herd-lite)
path=("${HOME}/.config/herd-lite/bin" $path)
export PHP_INI_SCAN_DIR="${HOME}/.config/herd-lite/bin:${PHP_INI_SCAN_DIR}"

# Ruby (RVM)
path+=("${HOME}/.rvm/bin")
[[ -s "${HOME}/.rvm/scripts/rvm" ]] && source "${HOME}/.rvm/scripts/rvm"

# Rust (Cargo)
export CARGO_ROOT="${HOME}/.cargo"
if [[ -d "${CARGO_ROOT}/bin" ]]; then
    path=("${CARGO_ROOT}/bin" $path)
fi

# Go
if [[ -d "${HOME}/go/bin" ]]; then
    path+=("${HOME}/go/bin")
fi

# Haskell (GHCup)
[[ -f "${HOME}/.ghcup/env" ]] && source "${HOME}/.ghcup/env"

# Ruby
command -v rbenv >/dev/null 2>&1 && eval "$(rbenv init - --no-rehash zsh)"

typeset -U path PATH
