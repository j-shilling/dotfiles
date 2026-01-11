# POSIX shell profile - sourced by login shells
# This file should remain POSIX-compliant (no bashisms)

# XDG Base Directory specification
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_RUNTIME_DIR="/run/user/${UID}"

# Password store location
export PASSWORD_STORE_DIR="${XDG_STATE_HOME}/password-store"

# Add user bin directories to PATH
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Source bashrc for interactive bash shells
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi
