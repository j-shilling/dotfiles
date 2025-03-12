(use-modules (ice-9 popen)
             (ice-9 textual-ports)

             (guix channels)
             (guix gexp)

             (gnu services)

             (gnu home)
             (gnu home services)
             (gnu home services shepherd)
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

(define %shepherd-service
  (service home-shepherd-service-type
           (home-shepherd-configuration
            (services
             (list (shepherd-service
                    (provision '(guix-repl))
                    (start #~(make-forkexec-constructor
                              (list
                               "/home/jake/.config/guix/current/bin/guix" "repl" "--listen=tcp:37146")
                              #:environment-variables (cons "INSIDE_EMACS=1")))
                    (stop #~(make-kill-destructor))
                    (documentation "REPL to me, like lovers do")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotfiles

(define dotfile-services
  (list
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (source-directory root)
             (directories '("./files"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUIX Core

(define %locale
  (make-glibc-utf8-locales glibc #:locales (list "en_US")))

(define %channels
  (let ((%channels (path-in-root "channels.scm"))
        (%channels-lock (path-in-root "channels-lock.scm")))
    (primitive-load
     (if (file-exists? %channels-lock)
         %channels-lock
         %channels))))

(define %guix
  (guix-for-channels %channels))

(define guix-core-services
  (list
   (simple-service 'set-guix-channels
                   home-channels-service-type
                   %channels)
   (simple-service 'add-guix-package
                   home-profile-service-type
                   (list %guix))
   (simple-service 'add-foreign-distro-packages
                   home-profile-service-type
                   (list %locale nss-certs))
   (simple-service 'set-foreign-distro-env-vars
                   home-environment-variables-service-type
                   `(("GUIX_LOCPATH" . ,(file-append %locale "/lib/locale"))
                     ("SSL_CERT_DIR" . ,(file-append nss-certs "/etc/ssl/certs"))
                     ("SSL_CERT_FILE" . "${GUIX_PROFILE}/etc/ssl/certs/ca-certificates.crt")))
   (simple-service 'run-guix-repl
                   home-shepherd-service-type
                   (list (shepherd-service
                            (provision '(guix-repl))
                            (start #~(make-forkexec-constructor
                                      (list
                                       #$(file-append %guix "/bin/guix") "repl" "--listen=tcp:37146")
                                      #:environment-variables (cons "INSIDE_EMACS=1")))
                            (stop #~(make-kill-destructor))
                            (documentation "REPL to me, like lovers do"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XDG + Shell

(define %bash-completion
  (specification->package "bash-completion"))

(define %bash-completion-file
  (file-append %bash-completion "/share/bash-completion/bash_completion"))

(define shell-services
  (list
   ;; XDG
   (simple-service
    'xdg-base-directories-values
    home-xdg-base-directories-service-type
    (home-xdg-base-directories-configuration
     (state-home "$HOME/.local/var/lib")))
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration))

   ;; Bash
   (simple-service 'add-bash-completion
                   home-profile-service-type
                   (list %bash-completion))
   (service home-bash-service-type
           (home-bash-configuration
            (environment-variables
             '(("HISTFILE" . "$XDG_CACHE_HOME/.bash_history")
               ("HISTFILESIZE" . "100000")
               ("HISTIGNORE" . "ls:exit:history:clear")
               ("PASSWORD_STORE" . "$XDG_STATE_HOME/password-store")))
            (bashrc
             (list
              (mixed-text-file
               "shell-options"
               "# Shell Options\n"
               "shopt -s histappend\n"
               "shopt -s cmdhist\n"
               "shopt -s checkwinsize\n"
               "shopt -s autocd\n"
               "shopt -s cdable_vars\n"
               "shopt -s dirspell\n"
               "shopt -s cdspell\n"
               "shopt -s globstar\n"
               "shopt -s nocaseglob\n"
               "shopt -s checkhash\n"
               "shopt -s lithist\n")
              (mixed-text-file
               "sourcing"
               "# Sourcing\n"
               "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && source \"$EAT_SHELL_INTEGRATION_DIR/bash\"\n"
               "[[ -f " %bash-completion-file " ]] && source " %bash-completion-file "\n")
              (mixed-text-file
               "fzf-setup"
               "# FZF Shell Integration\n"
               "eval $(fzf --bash)\n")))))

   ;; Readline
   (service home-inputrc-service-type
            (home-inputrc-configuration
             (key-bindings
              '(("\\e[A" . "history-search-backward")
                ("\\e[B" . "history-search-forward")))
             (variables
              '(("bell-style" . "none")
                ("bind-tty-special-chars" . #t)
                ("blink-matching-paren" . #t)
                ("colored-completion-prefix" . #t)
                ("colored-stats" . #t)
                ("completion-display-width" . "0")
                ("completion-ignore-case" . #t)
                ("completion-map-case" . #t)
                ("enable-bracketed-paste" . #t)
                ("expand-tilde" . #t)
                ("match-hidden-files" . #t)
                ("show-all-if-ambiguous" . #t)
                ("show-mode-in-prompt" . #f)
                ("mark-symlinked-directories" . #t)
                ("editing-mode" . "emacs")
                ("keymap" . "emacs")
                ("meta-flag" . #t)
                ("convert-meta" . #f)
                ("output-meta" . #t)))))

   ;; GnuPG
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration

             (extra-content
              (string-join (list "allow-emacs-pinentry"
                                 "allow-loopback-pinentry")
                           "\n"))))

   ;; OpenSSH
   (service home-openssh-service-type
            (home-openssh-configuration
             (hosts (list (openssh-host (name "github.com")
                                        (user "j-shilling")
                                        (host-name "github.com")
                                        (identity-file "~/.ssh/id_github"))))))

   ;; Essential Packages
   (simple-service 'add-core-shell-packages
                   home-profile-service-type
                   (specifications->packages
                    '("file"
                      "ripgrep"
                      "fd"
                      "fzf"
                      "coreutils"
                      "git"
                      "git:send-email"
                      "awscli"
                      "make"
                      "password-store")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs

(define %emacs
  (if wayland? emacs-pgtk emacs))

(define %elisp-packages
  (specifications->packages
   '("emacs-geiser"
     "emacs-geiser-guile"
     "emacs-which-key"
     "emacs-corfu"
     "emacs-apheleia"
     "emacs-pinentry"
     "emacs-guix"
     "emacs-pyvenv"
     "emacs-pass"
     "emacs-smartparens"
     "emacs-haskell-mode"
     "emacs-haskell-snippets"
     "emacs-diminish"
     "emacs-marginalia"
     "emacs-ligature"
     "emacs-wgrep"
     "emacs-multiple-cursors"
     "emacs-vlf"
     "emacs-so-long"
     "emacs-gcmh"
     "emacs-yaml-mode"
     "emacs-no-littering"
     "emacs-apheleia"
     "emacs-diff-hl"
     "emacs-diredfl"
     "emacs-consult-notmuch"
     "emacs-consult-dir"
     "emacs-consult-org-roam"
     "emacs-consult-yasnippet"
     "emacs-devdocs"
     "emacs-web-mode"
     "emacs-lsp-booster"
     "emacs-eglot-booster"
     "emacs-envrc"
     "emacs-fontaine"
     "emacs-eat"
     "emacs-modus-themes"
     "emacs-eshell-syntax-highlighting"
     "emacs-vertico"
     "emacs-orderless")))

(define emacs-services
  (list
   (simple-service 'add-emacs-packages
                   home-profile-service-type
                   (cons* %emacs %elisp-packages))
   (simple-service 'add-emacs-server
                   home-shepherd-service-type
                   (list
                    (shepherd-service
                     (documentation
                      "Emacs server.  Use @code{emacsclient} to connect to it.")
                     (provision '(emacs-server))
                     (modules '((shepherd support)))
                     (start #~(make-systemd-constructor
                               (list #$(file-append %emacs "/bin/emacs") "--fg-daemon")
                               (list (endpoint
                                      (make-socket-address
                                       AF_UNIX
                                       (string-append %user-runtime-dir
                                                      "/emacs/server"))
                                      #:name 'emacs-server
                                      #:socket-directory-permissions #o700))
                               #:lazy-start? #false
                               #:log-file (string-append
                                           (getenv "XDG_STATE_HOME")
                                           "/log/emacs-server.log")))
                     (stop #~(make-systemd-destructor)))))))

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



(define %syncthing-service
  (simple-service 'syncthing
                  home-syncthing-service-type
                  (syncthing-configuration
                   (user "jake"))))



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
  (append
   (list %mcron-jobs-service
         %syncthing-service
         %add-font-packages
         %fontconfig-service)
   dotfile-services
   guix-core-services
   shell-services
   emacs-services))

(define %home-packages
  (append (list
           (@ (config packages node-xyz) devcontainers-cli-0.72.0))
          (specifications->packages
           (list
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
           "direnv"
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
