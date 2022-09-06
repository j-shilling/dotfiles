(define-module (jrs manifests sway)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
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
         "fuzzel")))

(define manifest
  (packages->manifest packages))

manifest
