DOTFILES_DIR ?= ${HOME}/dotfiles
TARGET ?= ${HOME}
STOW = stow -d $(DOTFILES_DIR) -t $(TARGET) --no-folding -v -R

PKGS = emacs git shell mail ssh utils agents claude codex copilot

.PHONY: all $(PKGS)

all: $(PKGS)

$(PKGS):
	$(STOW) $@
