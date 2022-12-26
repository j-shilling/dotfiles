(define-module (jrs config wm)
  #:use-module (rde features)
  #:use-module (rde features linux)
  #:use-module (rde features wm)
  #:use-module (rde features networking)
  #:use-module ((rde features base) :prefix base:)
  #:use-module (rde features fontutils)

  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnome)

  #:use-module (gnu services)
  #:use-module (gnu services networking)

  #:use-module (guix gexp)

  #:use-module (jrs features xorg)
  #:use-module (jrs features mako))

(define (feature-desktop-services)

  (let ((base (base:feature-desktop-services)))

    (define (get-system-services config)
      (let* ((base-getter (feature-system-services-getter base))
             (base-services (base-getter config)))
        (modify-services base-services
          (network-manager-service-type config =>
                                        (network-manager-configuration
                                         (inherit config)
                                         (vpn-plugins
                                          (list
                                           network-manager-vpnc
                                           network-manager-openvpn
                                           network-manager-openconnect)))))))

    (feature
     (name 'jrs-feature-desktop-services)
     (values (feature-values base))
     (home-services-getter (feature-home-services-getter base))
     (system-services-getter get-system-services))))


(define-public %desktop-features
  (list
   (feature-i3)
   (feature-polybar)

   (feature-mako)

   (feature-networking)
   (feature-desktop-services)
   (feature-fonts
    #:font-monospace (font "Fira Mono" #:size 14 #:weight 'regular)
    #:font-sans (font "Fira Sans" #:size 14 #:weight 'regular)
    #:font-packages (list font-fira-mono font-fira-sans)
    #:default-font-size 14)
   (feature-pipewire)
   (feature-sway
    #:xwayland? #t
    #:extra-config
    `((bindsym $mod+Shift+e exec swaymsg exit)
      (output DVI-D-1 resolution 1920x1080 position 0,0)
      (output DP-3 resolution 1920x1080 position 1920,0)))
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   (feature-swaylock)
   (feature-waybar
    #:transitions? #t
    #:waybar-modules
    (list
     (waybar-cpu)
     (waybar-memory)
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
