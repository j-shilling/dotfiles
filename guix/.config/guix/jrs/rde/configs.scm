(define-module (jrs rde configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features version-control)
  #:use-module (rde features wm)
  #:use-module (rde features gnupg)
  #:use-module (rde features password-utils)
  #:use-module (rde features docker)
  #:use-module (rde features linux)
  #:use-module (rde features fontutils)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features system)
  #:use-module (rde features xdg)
  #:use-module (rde features clojure)
  #:use-module (rde features markup)
  #:use-module (rde gexp)

  #:use-module (gnu packages)
  #:use-module (gnu packages fonts)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu system file-systems)
  #:use-module (gnu home-services-utils)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (jrs packages clojure-lsp)
  #:use-module (jrs packages mozilla)
  #:use-module (jrs packages linux)
  #:use-module (jrs packages freedesktop)
  #:use-module (jrs packages node)

  #:use-module (jrs rde features javascript)

  #:use-module ((jrs profiles default) :prefix default:)
  #:use-module ((jrs manifests core) :prefix core:)
  #:use-module (guix gexp)
  #:use-module (ice-9 match))

(define os-file-systems
  (list (file-system
         (mount-point "/boot/efi")
         (device (uuid "F12D-A33C" 'fat32))
         (type "vfat"))
        (file-system
         (mount-point "/")
         (device
          (uuid "5b3dce06-4eb1-47a3-9b33-f7aa7a2ed83c"
                'ext4))
         (type "ext4"))))

(define %user-features
  (list

   (feature-user-info
    #:user-name "jake"
    #:full-name "Jake Shilling"
    #:email "shilling.jake@gmail.com"
    #:user-groups '("wheel" "netdev" "audio" "lp" "video" "tty")
    #:user-initial-password-hash #f
    #:emacs-advanced-user? #t
    #:rde-advanced-user? #f)

   (feature-gnupg
    #:gpg-primary-key "0FCC8E6A96FF109F"
    #:gpg-smart-card? #f
    #:pinentry-flavor 'emacs
    #:ssh-keys
    '(("57CCEEB098F2AA6791BA6D8F4CEF32B3F147C678")
      ("E556265A9520AFE6C5BEC85C47B1ADB883CCBC91")
      ("57407F876080A07BE7455903C9F52FB2922C8C49")))

   (feature-password-store
    #:remote-password-store-url "git@gitlab.com:shilling.jake/password-store.git")))

(define %main-features
  (list
   (feature-host-info
    #:host-name "olympus"
    #:timezone "America/New_York")
   (feature-kernel
    #:kernel linux
    #:initrd microcode-initrd
    #:firmware (list linux-firmware))
   (feature-file-systems
    #:file-systems os-file-systems)

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

   (feature-base-services)
   (feature-desktop-services)
   (feature-docker)

   (feature-pipewire
    ;; #:pipewire pipewire
    ;; #:wireplumber wireplumber
    )
   (feature-sway
    ;; #:xdg-desktop-portal xdg-desktop-portal
    ;; #:xdg-desktop-portal-wlr xdg-desktop-portal-wlr
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
    (append `(,firefox)
            (map specification->package+output
                 '("bluez"
                   "flatpak"
                   "make"
                   "nyxt"
                   "ungoogled-chromium")))
    #:system-packages
    ;; (map car core:packages)
    (map specification->package+output
         '("nss-certs")))))

(define-public config
  (rde-config
   (features
    (append
     %user-features
     %main-features))))

(define-public os
  (rde-config-operating-system config))

(define-public home
  (rde-config-home-environment config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("home" home)
      ("system" os)
      (_ home))))

(dispatcher)
