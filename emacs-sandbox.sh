#!/usr/bin/env bash

guix shell -C --no-cwd \
    -E "^DISPLAY$" \
    -E "^XAUTHORITY$" \
    --expose="$XAUTHORITY" \
    --share=/tmp/.X11-unix/ \
    --share=$HOME/dotfiles/emacs=$HOME \
    -m $HOME/.config/guix/manifests/core.scm \
    -m $HOME/.config/guix/manifests/emacs.scm \
    -- emacs
