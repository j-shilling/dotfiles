.PHONY: all

all:
	stow --dir "${HOME}/dotfiles" --target "${HOME}" --verbose --no-fold .
