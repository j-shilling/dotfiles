;;
;; Special Thanks to https://github.com/nicolas-graves/dotfiles
;;

(when (current-filename)
  (add-to-load-path
   (dirname (current-filename))))

(use-modules
 (guix gexp)
 (ice-9 match)
 (ice-9 pretty-print))

(define (channel-content)
  "Generate the content of the `channels.scm' file."
  `(list
    (channel
     (name 'nonguix)
     (url
      "https://gitlab.com/nonguix/nonguix")
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
    (channel
     (name 'rde)
     (url "https://git.sr.ht/~abcdw/rde")
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
    (channel
     (name 'guix)
     (url "https://git.savannah.gnu.org/git/guix.git")
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define channels-file
  (plain-file
   "channels"
   (with-output-to-string
     (lambda ()
       (pretty-print (channel-content))))))

(use-modules
 (gnu system file-systems))

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

(use-modules
 (rde features base)
 (rde features gnupg)
 (rde features password-utils))

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

(use-modules
 (rde features)
 (rde features system)
 (rde features fontutils)
 (rde features tmux)
 (rde features shells)
 (rde features shellutils)
 (rde features ssh)
 (rde features version-control)
 (rde features xdg)
 (rde features emacs)
 (rde features emacs-xyz)
 (rde features clojure)
 (rde features markup)
 (rde features docker)
 (rde features linux)
 (rde features wm)

 (rde gexp)

 (gnu services)
 (gnu services base)
 (gnu services desktop)

 (gnu packages)
 (gnu packages fonts)

 (gnu home services)

 (nongnu packages linux)
 (nongnu system linux-initrd)

 (jrs packages clojure-lsp)
 (jrs packages node)
 (jrs packages mozilla)
 (jrs features javascript))

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

   (feature-base-services
    #:guix-substitute-urls
    (append (list "https://substitutes.nonguix.org")
            (@ (guix store) %default-substitute-urls))
    #:guix-authorized-keys
    (append (list (local-file "./public-keys/nonguix.pub"))
            (@ (gnu services base) %default-authorized-guix-keys))
    #:base-services
    (append
     (list
      (simple-service
       'channels-and-sources
       etc-service-type
       `(("channels.scm" ,channels-file))))))

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
