DOTFILES_DIR ?= ${HOME}/dotfiles
TARGET ?= ${HOME}
STOW = stow -d $(DOTFILES_DIR) -t $(TARGET) --no-folding -v -R

PKGS = emacs git shell utils agents claude copilot

.PHONY: all $(PKGS)

all: $(PKGS)

$(PKGS):
	$(STOW) $@
