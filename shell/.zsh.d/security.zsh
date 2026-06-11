# Security tools: SSH agent and GPG

export GPG_TTY="$(tty)"

if [[ -z "$SSH_AUTH_SOCK" ]] && command -v ssh-agent >/dev/null 2>&1; then
    local agent_dir="${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}"
    if [[ ! -d "$agent_dir" ]]; then
        mkdir -p "$agent_dir" 2>/dev/null || agent_dir="${TMPDIR:-/tmp}"
    fi
    local agent_env="${agent_dir%/}/ssh-agent.env"

    if [[ -r "$agent_env" ]]; then
        source "$agent_env" >/dev/null
    fi

    if [[ -z "$SSH_AUTH_SOCK" ]] || ! ssh-add -l >/dev/null 2>&1; then
        if ssh-agent -s >| "$agent_env"; then
            source "$agent_env" >/dev/null
        else
            rm -f "$agent_env"
        fi
    fi
fi
