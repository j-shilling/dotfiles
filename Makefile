DOTFILES_DIR ?= ${HOME}/dotfiles
TARGET ?= ${HOME}
STOW = stow -d $(DOTFILES_DIR) -t $(TARGET) --no-folding -v -R

PKGS = emacs git shell mail ssh utils agents
OPTIONAL_PKGS = iterm
MACOS_PKGS = $(PKGS) $(OPTIONAL_PKGS)

.PHONY: all macos $(PKGS) $(OPTIONAL_PKGS)

all: $(PKGS)

macos: $(MACOS_PKGS)

$(PKGS) $(OPTIONAL_PKGS):
	$(STOW) $@
