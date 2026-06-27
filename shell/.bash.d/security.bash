# Security tools: SSH agent and GPG

export GPG_TTY="$(tty)"
eval $(ssh-agent -s)

# Optional machine-local secrets (work-only env vars; never commit)
[ -r "${HOME}/.bash.d/secrets.bash" ] && . "${HOME}/.bash.d/secrets.bash"
