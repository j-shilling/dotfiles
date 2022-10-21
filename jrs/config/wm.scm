(define-module (jrs config wm)
  #:use-module (rde features linux)
  #:use-module (rde features wm)
  #:use-module (rde features base)
  #:use-module (rde features fontutils)

  #:use-module (gnu packages fonts)

  #:use-module (jrs packages linux)
  #:use-module (jrs packages freedesktop))

(define-public %desktop-features
  (list
   (feature-desktop-services)
   (feature-fonts
     #:font-monospace (font "Fira Mono" #:size 14 #:weight 'regular)
     #:font-sans (font "Fira Sans" #:size 14 #:weight 'regular)
     #:font-packages (list font-fira-mono font-fira-sans)
     #:default-font-size 14)
   (feature-pipewire
     #:pipewire pipewire
     #:wireplumber wireplumber)
    (feature-sway
     #:xdg-desktop-portal xdg-desktop-portal
     #:xdg-desktop-portal-wlr xdg-desktop-portal-wlr
     #:xwayland? #t
     #:extra-config
     '((bindsym
        $mod+Shift+e exec
        swaynag -t
        warning -m "You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session."
        -B "Yes, exit sway" "swaymsg exit")))
    (feature-sway-run-on-tty
     #:sway-tty-number 2)
    (feature-sway-screenshot)
    (feature-swaylock)
    (feature-waybar
     #:transitions? #t
     #:waybar-modules
     (list
      (waybar-sway-window)
      (waybar-sway-workspaces
       #:format-icons '())
      (waybar-tray)
      (waybar-idle-inhibitor)
      (waybar-clock
       #:format "{:%I:%M %p}")
      (waybar-microphone)
      (waybar-volume
       #:show-percentage? #t)))))
