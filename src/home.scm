(use-modules (ice-9 popen)
             (ice-9 textual-ports)

             (guix channels)
             (guix gexp)

             (gnu services)

             (gnu home)
             (gnu home services)
             (gnu home services fontutils)
             (gnu home services ssh)
             (gnu home services dotfiles)
             (gnu home services guix)
             (gnu home services shells)
             (gnu home services xdg)
             (gnu home services mcron)
             (gnu home services syncthing)
             (gnu home-services version-control)
             (gnu home services gnupg)

             (gnu packages)
             (gnu packages base)
             (gnu packages emacs)
             (gnu packages certs)
             (gnu packages fonts)
             (gnu packages version-control)
             (gnu packages package-management))

(define wsl?
  (getenv "WSL_DISTRO_NAME"))

(define wayland?
  wsl?)

(define root
  (canonicalize-path
   (string-trim-right
    (get-string-all
     (open-pipe* OPEN_READ "git" "rev-parse" "--show-toplevel")))))

(define (path-in-root path)
  (if root
      (canonicalize-path
       (string-append root "/" path))))

(define channels
  (primitive-load (path-in-root "channels.scm")))

(define locale
  (make-glibc-utf8-locales glibc #:locales (list "en_US")))

;; Base

(define %guix-channels-service
  (simple-service 'set-guix-channels
                  home-channels-service-type
                  channels))

(define %dotfiles-service
  (service home-dotfiles-service-type
           (home-dotfiles-configuration
            (source-directory root)
            (directories '("./files")))))

(define %foreign-distro-packages-service
  (simple-service 'add-foreign-distro-packages
                  home-profile-service-type
                  (list locale nss-certs)))

(define %foreign-distro-env-vars-service
  (simple-service 'set-foreign-distro-env-vars
                  home-environment-variables-service-type
                  `(("GUIX_LOCPATH" . ,(file-append locale "/lib/locale"))
                    ("SSL_CERT_DIR" . ,(file-append nss-certs "/etc/ssl/certs"))
                    ("SSL_CERT_FILE" . "${GUIX_PROFILE}/etc/ssl/certs/ca-certificates.crt"))))

(define %xdg-base-directories-service
  (simple-service
   'xdg-base-directories-values
   home-xdg-base-directories-service-type
   (home-xdg-base-directories-configuration
    (state-home "$HOME/.local/var/lib"))))

(define %xdg-user-directories-service
  (service home-xdg-user-directories-service-type
           (home-xdg-user-directories-configuration)))

;; Appearance

(define %default-font-size 11)
(define %monospace-font
  `((name . "Iosevka")
    (package . ,font-iosevka)))
(define %serif-font
  `((name . "Iosevka Etoile")
    (package . ,font-iosevka-etoile)))
(define %sans-font
  `((name . "Iosevka Aile")
    (package . ,font-iosevka-aile)))
(define %unicode-font
  `((name . "Noto Emoji")
    (package . ,font-google-noto-emoji)))

(define %add-font-packages
  (simple-service
   'add-extra-fonts
   home-profile-service-type
   (map (lambda (f) (assq-ref f 'package))
        (list %monospace-font %serif-font %sans-font %unicode-font))))

(define %fontconfig-service
  (simple-service
   'add-fontconfig-font-families
   home-fontconfig-service-type
   (list
    `(alias
      (family "sans-serif")
      (prefer
       (family ,(assq-ref %sans-font 'name))))
    `(alias
      (family "serif")
      (prefer
       (family ,(assq-ref %serif-font 'name))))
    `(alias
      (family "monospace")
      (prefer
       (family ,(assq-ref %monospace-font 'name))))
    `(alias
      (family "emoji")
      (prefer
       (family ,(assq-ref %unicode-font 'name)))))))

;; Git, GPG, SSH

(define %git-package-service
  (simple-service
   'git-send-email-package
   home-profile-service-type
   (list
    git
    (list git "send-email"))))

(define %gpg-agent-service
  (service home-gpg-agent-service-type
           (home-gpg-agent-configuration
            
            (extra-content
             (string-join (list "allow-emacs-pinentry"
                                "allow-loopback-pinentry")
                          "\n")))))

(define %open-ssh-service
  (service home-openssh-service-type
           (home-openssh-configuration
            (hosts (list (openssh-host (name "github.com")
                                       (user "j-shilling")
                                       (host-name "github.com")
                                       (identity-file "~/.ssh/id_github")))))))

(define %syncthing-service
  (simple-service 'syncthing
                  home-syncthing-service-type
                  (syncthing-configuration
                   (user "jake"))))

(define %bash-service
  (service home-bash-service-type
           (home-bash-configuration
            (environment-variables
             '(("HISTFILE" . "$XDG_CACHE_HOME/.bash_history")))
            (bashrc
             (list
              (mixed-text-file "bash-settings"
                               (string-join '("HISTCONTROL=erasedups"
                                              "HISTFILESIZE=100000"
                                              "HISTIGNORE=ls:exit:history:clear"
                                              "shopt -s histappend"
                                              "shopt -s cmdhist"
                                              "shopt -s checkwinsize"
                                              "shopt -s autocd"
                                              "shopt -s cdable_vars"
                                              "shopt -s dirspell"
                                              "shopt -s cdspell"
                                              "shopt -s globstar"
                                              "shopt -s nocaseglob"
                                              "shopt -s checkhash"
                                              "shopt -s lithist"
                                              "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && source \"$EAT_SHELL_INTEGRATION_DIR/bash\"")
                                            "\n")))))))

(define %mcron-jobs-service
  (simple-service 'mcron-jobs
                  home-mcron-service-type
                  (list
                   #~(job '(next-minute (range 0 60 5))
                          (lambda ()
                            (system* "mbsync" "-Va"))
                          "mbsync")
                   #~(job '(next-minute (range 1 60 5))
                          (lambda ()
                            (system* "notmuch" "new"))
                          "notmuch"))))

(define %home-services
  (list %bash-service
        %mcron-jobs-service
        %foreign-distro-env-vars-service
        %foreign-distro-packages-service
        %git-package-service
        %xdg-base-directories-service
        %xdg-user-directories-service
        %guix-channels-service
        %dotfiles-service
        %syncthing-service
        %gpg-agent-service
        %open-ssh-service
        %add-font-packages
        %fontconfig-service))

(define %home-packages
  (append (list
           (if wayland? emacs-pgtk emacs)
           (guix-for-channels channels)
           (@ (config packages node-xyz) devcontainers-cli-0.72.0))
          (specifications->packages
           (list
           "file"
           "guile-colorized"
           "guile-next"
           "guile-readline"
           "tree-sitter-haskell"
           "tree-sitter-python"
           "tree-sitter-javascript"
           "tree-sitter-typescript"
           "tree-sitter-css"
           "tree-sitter-bash"
           "tree-sitter-dockerfile"
           "tree-sitter-json"
           "tree-sitter-markdown"
           "tree-sitter-markdown-gfm"
           "tree-sitter-nix"
           "tree-sitter-org"
           "tree-sitter-scheme"
           "tree-sitter-latex"
           "tree-sitter-html"
           "ripgrep"
           "fd"
           "direnv"
           "make"
           "font-gnu-unifont"
           "font-liberation"
           "isync"
           "msmtp"
           "notmuch"))))

(define-public he
  (home-environment
   (services %home-services)
   (packages %home-packages)))

he
