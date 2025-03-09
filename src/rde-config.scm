(define-module (rde-config)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)

  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features gnupg)
  #:use-module (rde features version-control)
  #:use-module (rde features documentation)
  #:use-module (rde features ssh)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features password-utils)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:use-module (rde features xdg)
  #:use-module (rde features guile)
  #:use-module (rde features lisp)
  #:use-module (rde features markup)
  #:use-module (rde features terminals)
  #:use-module (rde features fontutils)
  #:use-module (rde features docker)
  #:use-module (rde features python)

  #:use-module (rde home services emacs)

  #:use-module (rde gexp)

  #:use-module (rde packages emacs-xyz)
  #:use-module (rde packages guile-xyz)

  #:use-module (guix channels)
  #:use-module (guix gexp)

  #:use-module (gnu services)

  #:use-module (gnu home services)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home services gnupg)

  #:use-module (gnu packages base)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages file)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages shellutils)

  #:export (config))

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

(define home-packages
  (list file
        (guix-for-channels channels)
        guile-colorized
        guile-next
        guile-readline
        tree-sitter-haskell
        tree-sitter-python
        tree-sitter-javascript
        tree-sitter-typescript
        tree-sitter-css
        tree-sitter-bash
        tree-sitter-dockerfile
        tree-sitter-json
        tree-sitter-markdown
        tree-sitter-markdown-gfm
        tree-sitter-nix
        tree-sitter-org
        tree-sitter-scheme
        tree-sitter-latex
        tree-sitter-html
        ripgrep
        fd
        direnv
        gnu-make
        font-gnu-unifont
        font-liberation
        (@ (config packages node-xyz) devcontainers-cli-0.72.0)))

;; TODO:
;; - https://github.com/Qkessler/consult-project-extra/
;; - Maybe TempEL:
;;   - https://github.com/Crandel/tempel-collection
;;   - https://github.com/minad/tempel
(define elisp-packages
  (list emacs-geiser-latest
        emacs-geiser-guile-latest
        emacs-guix-latest
        emacs-pyvenv
        emacs-haskell-mode
        emacs-haskell-snippets
        emacs-diminish
        emacs-ligature
        emacs-wgrep
        emacs-multiple-cursors
        emacs-vlf
        emacs-so-long
        emacs-gcmh
        emacs-yaml-mode
        emacs-no-littering
        emacs-apheleia
        emacs-diff-hl
        emacs-diredfl
        emacs-consult-notmuch
        emacs-consult-dir
        emacs-consult-org-roam
        emacs-consult-yasnippet
        emacs-devdocs
        emacs-web-mode
        emacs-lsp-booster
        emacs-eglot-booster
        emacs-vterm
        emacs-envrc
        emacs-fontaine
        (@ (config packages emacs-xyz) emacs-codeium)))

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
            (pinentry-program
             (file-append pinentry-emacs "/bin/pinentry-emacs")))))

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
            (package bash)
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
                                              "shopt -s dirspell"
                                              "shopt -s cdspell"
                                              "shopt -s globstar"
                                              "shopt -s nocaseglob")
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

(define custom-home-services
  (list %bash-service
        %mcron-jobs-service
        %foreign-distro-env-vars-service
        %foreign-distro-packages-service
        %git-package-service
        %guix-channels-service
        %dotfiles-service
        %syncthing-service
        %gpg-agent-service
        %open-ssh-service
        %add-font-packages
        %fontconfig-service))

(define-public config
  (rde-config
   (features
    (list
     ;; Base Features
     (feature-user-info
      #:user-name "jake"
      #:full-name "Jake Shilling"
      #:email "shilling.jake@gmail.com"
      #:emacs-advanced-user? #t)
     (feature-custom-services
      #:home-services custom-home-services)
     (feature-base-packages
      #:home-packages `(,@home-packages ,@elisp-packages))
     (feature-xdg)
     ;; Messaging
     (feature-mail-settings
      #:mail-accounts
      (list
       (mail-account
        (id 'personal)
        (type 'gmail)
        (fqda "shilling.jake@gmail.com")
        (pass-cmd "pass show mail/shilling.jake@gmail.com"))))
     (feature-irc-settings
      #:irc-accounts
      (list
       (irc-account
        (id 'libera)
        (network "irc.libera.chat")
        (bouncer? #f)
        (nick "Jacobissimus"))))
     (feature-emacs-erc
      #:erc-log? #t
      #:erc-kill-buffers-on-quit? #t
      #:erc-align-nicknames? #t
      #:erc-images? #t
      #:erc-autojoin-channels-alist
      '(("irc.libera.chat" . ("#guix"
                              "#rde"
                              "#emacs"
                              "#emacs-beginners"
                              "#emacs-til"
                              "#emacs-social"
                              "#lisp"
                              "#scheme"
                              "#clojure"))))
     (feature-isync)
     (feature-msmtp)
     (feature-emacs-message)
     (feature-emacs-org-mime)
     (feature-notmuch)

     ;; Emacs
     (feature-emacs
      #:emacs (if wayland? emacs-pgtk emacs)
      #:emacs-server-mode? (not wsl?)
      #:default-terminal? #f
      #:default-application-launcher? #f)
     (feature-emacs-appearance)
     (feature-emacs-modus-themes
      #:dark? #t)
     (feature-emacs-which-key)
     (feature-emacs-tramp)
     (feature-emacs-dired)
     (feature-emacs-eshell)
     (feature-emacs-completion)
     (feature-emacs-vertico)
     (feature-emacs-corfu)
     (feature-emacs-project)
     (feature-emacs-ace-window)
     (feature-emacs-smartparens)
     (feature-emacs-eglot)
     (feature-emacs-dape)
     (feature-emacs-flymake)
     (feature-emacs-git)
     (feature-emacs-elisp)
     (feature-emacs-guix)
     (feature-emacs-xref)
     (feature-emacs-pdf-tools)
     (feature-emacs-help)
     (feature-emacs-info)
     (feature-emacs-devdocs)
     (feature-emacs-org)
     (feature-emacs-org-roam
      #:org-roam-directory "~/org/roam"
      #:org-roam-todo? #t)
     (feature-emacs-citation)
     (feature-emacs-zotra)
     (feature-emacs-spelling)

     ;; Tools
     (feature-docker)

     ;; Language Specific
     (feature-markdown)
     (feature-tex)
     (feature-guile
      #:emacs-arei emacs-arei-latest
      #:guile-ares-rs guile-ares-rs-latest)
     (feature-python
      #:python python-3.12
      #:black? #t)
     (feature-lisp
      #:extra-lisp-packages
      (list
       (@ (config packages lisp-xyz) sbcl-cl-transducers)
       sbcl-quickproject
       sbcl-fiveam
       sbcl-trivia
       sbcl-alexandria
       sbcl-check-it))))))

config
