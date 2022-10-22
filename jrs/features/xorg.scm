(define-module (jrs features xorg)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)

  #:use-module (gnu home services)

  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)

  #:use-module (guix gexp)

  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde serializers elisp)

  #:use-module (jrs services xorg)

  #:use-module (srfi srfi-1)

  #:export (feature-i3
            feature-polybar))

(define* (feature-i3
          #:key
          (i3 i3-gaps)
          (dmenu dmenu)
          (alacritty alacritty)
          (i3-mod 'Mod4)
          (xorg-configuration (xorg-configuration))
          (extra-config '()))
  "Setup and configure i3."
  (ensure-pred any-package? i3-gaps)
  (ensure-pred any-package? alacritty)
  (ensure-pred any-package? dmenu)

  (define (home-xorg-services config)
    (let* ((lock-cmd
            (get-value 'default-screen-locker config "loginctl lock-session"))

           (default-terminal
             (get-value-eval 'default-terminal config
                             (file-append alacritty "/bin/alacritty")))
           (backup-terminal
            (get-value 'backup-terminal config
                       (file-append alacritty "/bin/alacritty")))
           (default-application-launcher
             (get-value 'default-application-launcher config
                        (file-append dmenu "/bin/dmenu_run -l 20 -p run:"))))
      (list
       (service home-i3-service-type
                (home-i3-configuration
                 (package i3)
                 (config
                  `((,#~"\n\n# General settings:")
                    (set $mod ,i3-mod)

                    (bindsym $mod+Shift+r reload)
                    (bindsym $mod+Shift+e exec i3-msg exit)

                    (,#~"\n\n# Launching external applications:")
                    (set $term ,default-terminal)
                    (set $backup-term ,backup-terminal)
                    (set $menu ,default-application-launcher)
                    (set $lock ,lock-cmd)

                    ,@(if (get-value 'default-pass-prompt-fn config)
                          `((set $pass ,(get-value-eval 'default-pass-prompt-fn config))
                            (bindsym $mod+Shift+p exec $pass))
                          '())

                    (bindsym $mod+Control+Shift+Return exec $backup-term)
                    (bindsym $mod+Return exec $term)

                    (bindsym $mod+Shift+d exec $menu)
                    (bindsym $mod+Shift+l exec $lock)

                    (,#~"\n\n# Manipulating windows:")
                    (bindsym $mod+Shift+c kill)
                    (bindsym $mod+Shift+f fullscreen)
                    (bindsym $mod+Shift+space floating toggle)
                    (bindsym $mod+Ctrl+space focus mode_toggle)

                    (bindsym $mod+Left focus left)
                    (bindsym $mod+Down focus down)
                    (bindsym $mod+Up focus up)
                    (bindsym $mod+Right focus right)

                    (bindsym $mod+Shift+Left move left)
                    (bindsym $mod+Shift+Down move down)
                    (bindsym $mod+Shift+Up move up)
                    (bindsym $mod+Shift+Right move right)

                    (,#~"\n\n# Moving around workspaces:")
                    (bindsym $mod+tab workspace back_and_forth)
                    ,@(append-map
                       (lambda (x)
                         `((bindsym ,(format #f "$mod+~a" (modulo x 10))
                                    workspace number ,x)
                           (bindsym ,(format #f "$mod+Shift+~a" (modulo x 10))
                                    move container to workspace number ,x)))
                       (iota 10 1))

                    (,#~"\n\n# Scratchpad settings:")
                    (bindsym $mod+Shift+minus move scratchpad)
                    (bindsym $mod+minus scratchpad show)

                    (,#~"")
                    (default_border pixel)
                    (default_floating_border pixel)
                    (gaps inner ,(get-value 'emacs-margin config 8))))))

       (simple-service
        'xinitrc-for-xorg
        home-files-service-type
        `((".xinitrc"
           ,(mixed-text-file
             "xinitrc"
             "exec " i3 "/bin/i3"))))

       (simple-service
        'packages-for-i3
        home-profile-service-type
        (append
         (if (and (get-value 'default-terminal config)
                  (get-value 'backup-terminal config))
             '() (list alacritty))
         (if (get-value 'default-application-launcher config)
             '() (list dmenu))
         (list xinit))))))

  (define (system-xorg-services _)
    (list
     (service xorg-server-service-type
              xorg-configuration)))

  (feature
   (name 'i3)
   (values `((i3 . ,i3)
             (xorg . #t)
             (xorg-configuration . ,xorg-configuration)))
   (home-services-getter home-xorg-services)
   (system-services-getter system-xorg-services)))

(define* (polybar-module
          name
          #:optional
          (config '())
          #:key
          (placement 'modules-right)
          (bar-id 'main))
  (simple-service
   (symbol-append 'polybar-module name)
   home-polybar-service-type
   (home-polybar-extension
    (config
     `((,(symbol-append 'bar/ bar-id) ((,placement . ,name)))
       (,(symbol-append 'module/ name) ,config))))))

(define* (feature-polybar
          #:key
          (polybar polybar))

  (define (home-polybar-services _)
    (list
     (service
      home-polybar-service-type
      (home-polybar-configuration
       (polybar polybar)
       (config
        `((colors ((base01 . ,(string->symbol "#282828"))
                   (base02 . ,(string->symbol "#383838"))
                   (base03 . ,(string->symbol "#585858"))
                   (base04 . ,(string->symbol "#b8b8b8"))
                   (base05 . ,(string->symbol "#d8d8d8"))
                   (base07 . ,(string->symbol "#f8f8f8"))
                   (base08 . ,(string->symbol "#ab4642"))))

          (bar/main ((background . ,(string->symbol "${colors.base01}"))
                     (foreground . ,(string->symbol "${colors.base04}"))))))))))

  (feature
   (name 'polybar)
   (values `((polybar . ,polybar)
             (i3-statusbar . #t)))
   (home-services-getter home-polybar-services)))
