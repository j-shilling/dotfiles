(define-module (jrs config base))

(define-public %main-features
  (list
   (feature-custom-services
    #:home-services
    (list

     (simple-service
      'extend-environment-variables
      home-environment-variables-service-type
      `(("GUILE_LOAD_PATH" . (string-append
                              "${GUILE_LOAD_PATH}:"
                              "${HOME}/.config/guix/current/share/guile/site/3.0"))
        ("XDG_DATA_DIRS" . (string-append
                            "${XDG_DATA_DIRS}:"
                            "/var/lib/flatpak/exports/share:"
                            "${HOME}/.local/share/flatpak/exports/share")))))
    #:system-services
    (list
     (simple-service
       'channels-and-sources
       etc-service-type
       `(("channels.scm" ,channels-file)))
     (service guix-publish-service-type
              (guix-publish-configuration
               (advertise? #t)))
     (service bluetooth-service-type
              (bluetooth-configuration))))

   (feature-fonts
    #:font-monospace (font "Fira Mono" #:size 14 #:weight 'regular)
    #:font-sans (font "Fira Sans" #:size 14 #:weight 'regular)
    #:font-packages (list font-fira-mono font-fira-sans)
    #:default-font-size 14)

   (feature-tmux)
   (feature-bash)
   (feature-direnv)
   (feature-ssh)
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-git)

   (feature-xdg)

   (feature-isync)
   (feature-notmuch)
   (feature-msmtp)

   (feature-emacs
    #:additional-elisp-packages
    (map specification->package+output
         '("emacs-paredit"
           "emacs-typescript-mode"))
    #:extra-init-el
    `(,(slurp-file-like (local-file "./elisp/configure-defaults.el"))
      ,(slurp-file-like (local-file "./elisp/configure-lisp.el"))
      ,(slurp-file-like (local-file "./elisp/configure-javascript.el"))))

   (feature-emacs-appearance
    #:dark? #t)
   (feature-emacs-faces)
   (feature-emacs-tramp)
   (feature-emacs-completion)
   (feature-emacs-corfu)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-which-key)
   (feature-emacs-keycast
    #:turn-on? #f)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-git)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-eglot)

   (feature-emacs-pdf-tools)
   (feature-emacs-org)
   (feature-emacs-org-agenda)

   (feature-clojure
    #:clojure-lsp
    clojure-lsp)
   (feature-javascript
    #:node-pkg node)
   (feature-markdown)

   (feature-base-services
    #:guix-substitute-urls
    (append (list "https://substitutes.nonguix.org")
            (@ (guix store) %default-substitute-urls))
    #:guix-authorized-keys
    (append (list (local-file "./public-keys/nonguix-key.pub"))
            (@ (gnu services base) %default-authorized-guix-keys)))

   (feature-desktop-services)
   (feature-docker)

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
      #:show-percentage? #t)))

   (feature-base-packages
    #:home-packages
    (map specification->package+output
         '("bluez"
           "zoom"
           "flatpak"
           "make"
           "nyxt"
           "ungoogled-chromium"
           "zathura"
           "zathura-pdf-poppler"))
    #:system-packages
    ;; (map car core:packages)
    (map specification->package+output
         '("nss-certs")))))
