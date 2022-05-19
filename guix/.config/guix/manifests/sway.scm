(define-module (manifests sway)
  #:use-module (guix profiles)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages terminals)
  #:export (manifest
            packages))

(define packages
  (list sway
        font-fira-sans
        font-fira-code
        font-fira-mono
        bemenu
        waybar
        alacritty))

(define manifest
  (packages->manifest packages))

manifest
