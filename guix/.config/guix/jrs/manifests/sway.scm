(define-module (jrs manifests sway)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:use-module (jrs packages fuzzel)
  #:export (packages
            manifest))

(define packages
  (append
   (map (compose list specification->package+output)
        '("alacritty"
          "bemenu"
          "brightnessctl"
          "font-fira-code"
          "font-fira-mono"
          "font-fira-sans"
          "mako"
          "pavucontrol"
          "sway"
          "waybar"
          "pipewire"
          "wireplumber"
          "xdg-desktop-portal"
          "xdg-desktop-portal-wlr"
          "slurp"
          "grim"
          "grimshot"))
   (list `(,fuzzel "out"))))

(define manifest
  (packages->manifest packages))

manifest
