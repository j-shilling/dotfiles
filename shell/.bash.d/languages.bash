# Language runtime managers and toolchains

# Python (PyEnv)
export PYENV_ROOT="${HOME}/.pyenv"
if [[ -d "${PYENV_ROOT}/bin" ]]; then
    export PATH="${PYENV_ROOT}/bin:${PATH}"
fi
command -v pyenv >/dev/null && eval "$(pyenv init - bash)"

# Node.js (NVM)
export NVM_DIR="${XDG_CONFIG_HOME:-${HOME}/.config}/nvm"
if [[ -s "$NVM_DIR/nvm.sh" ]]; then
    source "$NVM_DIR/nvm.sh"
elif [[ -s "/opt/homebrew/opt/nvm/nvm.sh" ]]; then
    export NVM_DIR="${HOME}/.nvm"
    source "/opt/homebrew/opt/nvm/nvm.sh"
fi
if [[ -s "$NVM_DIR/bash_completion" ]]; then
    source "$NVM_DIR/bash_completion"
elif [[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ]]; then
    source "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
fi

# Node.js (pnpm)
export PNPM_HOME="${HOME}/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# PHP (Herd-lite)
export PATH="${HOME}/.config/herd-lite/bin:$PATH"
export PHP_INI_SCAN_DIR="${HOME}/.config/herd-lite/bin:$PHP_INI_SCAN_DIR"

# Ruby (RVM)
export PATH="$PATH:${HOME}/.rvm/bin"
[[ -s "${HOME}/.rvm/scripts/rvm" ]] && source "${HOME}/.rvm/scripts/rvm"

# Rust (Cargo)
export CARGO_ROOT="${HOME}/.cargo"
if [[ -d "${CARGO_ROOT}/bin" ]]; then
    export PATH="${CARGO_ROOT}/bin:${PATH}"
fi

# Go
if [[ -d "${HOME}/go/bin" ]]; then
    export PATH="${PATH}:${HOME}/go/bin"
fi

# Haskell (GHCup)
[ -f "${HOME}/.ghcup/env" ] && source "${HOME}/.ghcup/env"

# Ruby
command -v rbenv >/dev/null && eval "$(rbenv init - --no-rehash bash)"
