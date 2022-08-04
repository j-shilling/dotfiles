#!/usr/bin/env bash

guix environment \
    --pure \
    --no-cwd \
    --container \
    --preserve="^DISPLAY$" \
    --share=/tmp/.X11-unix/ \
    --share="$HOME/dotfiles/emacs/.emacs.d/=$HOME/.emacs.d" \
    --share="$HOME/.config/guix/=$HOME/.config/guix" \
    --manifest="$HOME/.config/guix/manifests/emacs.scm" -- emacs --debug-init
