# POSIX shell profile - sourced by login shells
# This file should remain POSIX-compliant (no bashisms)

# XDG Base Directory specification
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_STATE_HOME="${HOME}/.local/state"

case "$(uname -s 2>/dev/null)" in
    Darwin)
        if [ -n "${TMPDIR:-}" ] && { [ -z "${XDG_RUNTIME_DIR:-}" ] || [ ! -d "${XDG_RUNTIME_DIR}" ]; }; then
            export XDG_RUNTIME_DIR="${TMPDIR%/}"
        fi
        ;;
    *)
        if [ -z "${XDG_RUNTIME_DIR:-}" ]; then
            export XDG_RUNTIME_DIR="/run/user/${UID}"
        fi
        ;;
esac

# Certificate bundle overrides
if [ -d "${XDG_DATA_HOME}/certs" ]; then
    export CERT_DIR="${XDG_DATA_HOME}/certs"
    export ZSCALER_CERT_PATH="${CERT_DIR}/zscaler.pem"
    export CERT_PATH="${CERT_DIR}/ca-bundle.pem"

    if [ -r "${CERT_PATH}" ]; then
        export SSL_CERT_FILE="${CERT_PATH}"
        export SSL_CERT_DIR="${CERT_DIR}"
        export REQUESTS_CA_BUNDLE="${CERT_PATH}"
    fi

    if [ -r "${ZSCALER_CERT_PATH}" ]; then
        export NODE_EXTRA_CA_CERTS="${ZSCALER_CERT_PATH}"
    fi
fi

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
