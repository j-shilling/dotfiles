(define-module (manifests sway)
  #:use-module (guix profiles)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:export (manifest
            packages))

(define packages
  (list alacritty
        bemenu
        brightnessctl
        font-fira-code
        font-fira-mono
        font-fira-sans
        mako
        pavucontrol
        sway
        waybar))

(define manifest
  (packages->manifest packages))

manifest
