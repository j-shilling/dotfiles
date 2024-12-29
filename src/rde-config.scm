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

  #:use-module (guix channels)
  #:use-module (guix gexp)

  #:use-module (gnu services)

  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services mcron)

  #:use-module (gnu packages base)
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
  (let ((maybe-profile (getenv "GUIX_PROFILE")))
    (if maybe-profile
        (profile-channels maybe-profile)
        (primitive-load (path-in-root "channels.scm")))))

(define locale
  (make-glibc-utf8-locales glibc #:locales (list "en_US")))

(define home-packages
  (list file
        guile-colorized
        guile-next
        guile-readline
        poetry
        tree-sitter-haskell
        tree-sitter-python))

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
        (@ (config packages emacs-xyz) emacs-codeium)))

(define custom-home-services
  `(,(simple-service 'set-locale-path
                     home-environment-variables-service-type
                     `(("GUIX_LOCPATH" . ,(file-append locale "/lib/locale"))))
    ,@(if channels
          (list
           (simple-service 'set-guix-channels
                           home-channels-service-type
                           channels))
          (list))
    ,(service home-bash-service-type
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
                                               "\n"))))))
    ,(simple-service 'mcron-jobs
                     home-mcron-service-type
                     (list
                      #~(job '(next-minute (range 0 60 5))
                             (lambda ()
                               (system* "mbsync" "-Va"))
                             "mbsync")
                      #~(job '(next-minute (range 1 60 5))
                             (lambda ()
                               (system* "notmuch" "new"))
                             "notmuch")))
    ,(simple-service 'base-emacs-config
                    home-emacs-service-type
                    (home-emacs-extension
                     (elisp-packages elisp-packages)
                     (init-el
                      `(,(slurp-file-like (local-file (path-in-root "files/emacs/init.el")))))))))

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
     ((@ (config features base) feature-foreign-distro)
      #:glibc-locales locale)
     (feature-custom-services
      #:home-services custom-home-services)
     (feature-base-packages
      #:home-packages home-packages)
     (feature-xdg)
     (feature-fonts)

     ;; Shell     
     (feature-vterm)
     (feature-manpages)
     (feature-direnv)
     (feature-gnupg
      #:gpg-primary-key "0FCC8E6A96FF109F"
      #:ssh-keys
      '(("E556265A9520AFE6C5BEC85C47B1ADB883CCBC91")))
     (feature-ssh)
     (feature-git
      #:sign-commits? #t
      #:git-send-email? #t
      #:extra-config
      `((core
         ((autocrlf . "input")
          (whitespace . "-trailing-space,-space-before-tab,-cr-at-eol")))
        (init
         ((defaultBranch . "main")))
        (merge
         ((log . #t)
          (renormalize . #t)
          (ff . #f)
          (renames . #t)))
        (pull
         ((rebase . #t)
          (ff . #f)
          (autoSetupRemote . #t)))
        (fetch
         ((prune . #t)))))
     (feature-password-store
      #:remote-password-store-url "git@github.com:j-shilling/password-store.git")

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
     (feature-emacs-all-the-icons)
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
     (feature-emacs-spelling)

     ;; Tools
     ((@ (config features aws) feature-aws))
     (feature-docker)
     (feature-compile)

     ;; Language Specific
     (feature-markdown)
     (feature-tex)
     (feature-guile)
     (feature-python
      #:python python-3.12
      #:black? #t)
     (feature-lisp
      #:extra-lisp-packages
      (list
       cl-asdf
       cl-quickproject
       cl-fiveam
       cl-trivia
       cl-alexandria
       cl-check-it))))))

config
