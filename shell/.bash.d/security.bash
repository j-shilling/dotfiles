# Security tools: SSH agent and GPG

export GPG_TTY="$(tty)"
eval $(ssh-agent -s)
