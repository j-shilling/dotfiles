# Security tools: SSH agent and GPG

export GPG_TTY="$(tty)"

if [[ -z "$SSH_AUTH_SOCK" ]] && command -v ssh-agent >/dev/null 2>&1; then
    local agent_env="${XDG_RUNTIME_DIR:-/tmp}/ssh-agent.env"

    if [[ -r "$agent_env" ]]; then
        source "$agent_env" >/dev/null
    fi

    if [[ -z "$SSH_AUTH_SOCK" ]] || ! ssh-add -l >/dev/null 2>&1; then
        ssh-agent -s >| "$agent_env"
        source "$agent_env" >/dev/null
    fi
fi
