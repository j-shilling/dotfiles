# OpenRouter API key via password-store
if command -v pass >/dev/null 2>&1; then
    OPENROUTER_API_KEY="$(pass show pathable/openrouter-key 2>/dev/null)"
    export OPENROUTER_API_KEY

    if [[ -n "$OPENROUTER_API_KEY" ]]; then
        export ANTHROPIC_BASE_URL="https://openrouter.ai/api"
        export ANTHROPIC_AUTH_TOKEN="$OPENROUTER_API_KEY"
        export ANTHROPIC_API_KEY=""
        alias codex="codex --profile openrouter"
    fi
fi
