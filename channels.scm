;; -*- mode: scheme -*-
(use-modules (guix ci)
             (guix channels))

(list
 %default-guix-channel
 (channel
  (name 'dotfiles)
  (url "file:///home/jake/dotfiles")))
