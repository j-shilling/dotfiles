# Bash shell options (shopt)

shopt -s histappend        # Append to history file, don't overwrite
shopt -s cmdhist           # Save multi-line commands as single history entry
shopt -s checkwinsize      # Update LINES and COLUMNS after each command
shopt -s autocd            # Auto-cd to directory if command is a directory name
shopt -s cdable_vars       # Try variable expansion on cd argument
shopt -s dirspell          # Autocorrect directory spelling on cd
shopt -s cdspell           # Autocorrect minor spelling errors on cd
shopt -s globstar          # Enable ** for recursive globbing
shopt -s nocaseglob        # Case-insensitive globbing
shopt -s checkhash         # Check hash table before command execution
shopt -s lithist           # Save multi-line commands with embedded newlines
