(define-module (jrs features xorg)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)

  #:use-module (gnu home services)

  #:use-module (gnu packages xorg)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages pulseaudio)

  #:use-module (guix gexp)

  #:use-module (rde features)
  #:use-module (rde features fontutils)
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
    (let* ((font-sans (get-value 'font-sans config))
           (font (format #f "pango:~a ~a"
                         (font-name font-sans)
                         (font-size font-sans)))
           (lock-cmd
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
       (simple-service
        'i3-launch-shepherd
        home-i3-service-type
        `((,#~"\n\n# Launch shepherd:")
          (exec ,(program-file
                  "launch-shepherd"
                  #~(let ((log-dir (or (getenv "XDG_LOG_HOME")
                                       (format #f "~a/.local/var/log"
                                               (getenv "HOME")))))
                      (system*
                       #$(file-append shepherd "/bin/shepherd")
                       "--logfile"
                       (string-append log-dir "/shepherd.log")))))))

       (simple-service
        'i3-dbus-update-activation-environment
        home-i3-service-type
        `(,@(if (get-value 'dbus config)
                `((,#~"\n\n# Update dbus environment variables:")
                  (exec ,(file-append
                          (get-value 'dbus config)
                          "/bin/dbus-update-activation-environment") --all))
                '())))
       (service home-i3-service-type
                (home-i3-configuration
                 (package i3)
                 (config
                  `((,#~"\n\n# General settings:")
                    (font ,font)
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
                    (bindsym $mod+Tab workspace back_and_forth)
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

             ;; Unset all the stuff set by feature-sway
             "unset XDG_CURRENT_DESKTOP\n"
             "unset XDG_SESSION_TYPE\n"
             "unset RTC_USE_PIPEWIRE\n"
             "unset SDL_VIDEODRIVER\n"
             "unset MOZ_ENABLE_WAYLAND\n"
             "unset CLUTTER_BACKEND\n"
             "unset ELM_ENGINE\n"
             "unset ECORE_EVAS_ENGINE\n"
             "unset QT_QPA_PLATFORM\n"

             "xsetroot -solid dimgray\n"
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
         (list xinit xsetroot))))))

  (define (system-xorg-services _)
    (list
     (service xorg-server-service-type
              xorg-configuration)))

  (feature
   (name 'i3)
   (values `((i3 . ,i3)
             (xorg . #t)
             (xorg-configuration . ,xorg-configuration)
             (xinit . ,xinit)))
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

  (define (home-polybar-services config)

    (define font-mono
      (and=> (get-value 'font-monospace config)
             font-name))

    (list
     (service
      home-polybar-service-type
      (home-polybar-configuration
       (polybar polybar)
       (config
        (let* ((i3-label-padding 1)
               (font-size 14)
               (font-offset 5)
               (height (string->symbol
                        (format #f "~Apt"
                                (+ font-size (* font-offset 2)))))
               (font-0 (string->symbol
                        (format #f "~A:style=Regular:size=~A;~A"
                                font-mono font-size font-offset))))
          `((colors ((base01 . ,(string->symbol "#282828"))
                     (base02 . ,(string->symbol "#383838"))
                     (base03 . ,(string->symbol "#585858"))
                     (base04 . ,(string->symbol "#b8b8b8"))
                     (base05 . ,(string->symbol "#d8d8d8"))
                     (base07 . ,(string->symbol "#f8f8f8"))
                     (base08 . ,(string->symbol "#ab4642"))))

            (bar/main ((background . ,(string->symbol "${colors.base01}"))
                       (foreground . ,(string->symbol "${colors.base04}"))

                       (font-0 . ,font-0)
                       (font-1 . FontAwesome)

                       (enable-ipc . true)
                       (height . ,height)

                       (monitor . ,(string->symbol "${env:MONITOR:}"))

                       (modules-left . i3)
                       (modules-center . xwindow)
                       (modules-right . ,(string->symbol "pulseaudio date"))))

            (module/i3 ((type . internal/i3)
                        (pin-workspaces . true)
                        (enable-client . true)

                        (label-visible . %index%)
                        (label-visible-padding . ,i3-label-padding)

                        (label-focused . %index%)
                        (label-focused-foreground . ,(string->symbol "${colors.base07}"))
                        (label-focused-background . ,(string->symbol "${colors.base02}"))
                        (label-focused-padding . ,i3-label-padding)

                        (label-unfocused . %index%)
                        (label-unfocused-padding . ,i3-label-padding)

                        (label-urgent . %index%)
                        (label-urgent-foreground . ,(string->symbol "${colors.base08}"))))

            (module/xwindow ((type . internal/xwindow)))

            (module/date ((type . internal/date)
                          (date . "")
                          (time . ,(string->symbol "%l:%M %p"))
                          (date-alt . ,(string->symbol "%b %d, %Y"))
                          (time-alt . "")
                          (label . ,(string->symbol "%date%%time%"))
                          (label-padding . ,i3-label-padding)))

            (module/pulseaudio ((type . internal/pulseaudio)
                                (format-volume . ,(string->symbol "<ramp-volume> <label-volume>"))
                                (label-volume . %percentage%%)
                                ;; (label-muted . ,(string->symbol "%{T1}🔇%{T-}"))
                                (ramp-volume-0 . ,(string->symbol "%{T1}🔈%{T-}"))
                                (ramp-volume-1 . ,(string->symbol "%{T1}🔊%{T-}"))
                                (ramp-volume-2 . ,(string->symbol "%{T1}📢%{T-}"))
                                (on-click . ,#~(format #f "~s"
                                                       #$(file-append
                                                          (get-value 'pavucontrol config pavucontrol)
                                                          "/bin/pavucontrol"))))))))))
     (simple-service
      'i3-waybar
      home-i3-service-type
      `((exec_always --no-startup-id
                     ,(program-file
                       "launch-polybar"
                       #~(begin
                           (use-modules (guix build utils)
                                        (ice-9 popen)
                                        (ice-9 rdelim))

                           (system* "killall" "polybar")

                           (let ((polybar-bin (string-append #$polybar "/bin/polybar")))

                             (define (list-monitors)
                               (define lines '())
                               (let ((pipe (open-input-pipe (string-append polybar-bin " -m"))))
                                 (do ((line (read-line pipe)
                                            (read-line pipe)))
                                     ((eof-object? line))
                                   (set! lines (cons line lines)))
                                 (close-pipe pipe))

                               (map (lambda (str)
                                      (car (string-split str #\:)))
                                    lines))

                             (define (start-polybar-on-each-monitor monitors)
                               (map (lambda (monitor)
                                      (setenv "MONITOR" monitor)
                                      (let ((pid (primitive-fork)))
                                        (if (zero? pid)
                                            (execlp polybar-bin)
                                            (begin
                                              (unsetenv "MONITOR")
                                              pid))))
                                    monitors))

                             (for-each waitpid
                                       (start-polybar-on-each-monitor
                                        (list-monitors)))))))))
     (simple-service
      'polybar-add-font-package
      home-profile-service-type
      (list font-awesome))))

  (feature
   (name 'polybar)
   (values `((polybar . ,polybar)
             (i3-statusbar . #t)))
   (home-services-getter home-polybar-services)))
