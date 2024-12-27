(define-module (config tools shell)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages bash)
  #:use-module (rde features)
  #:use-module (rde features gnupg)
  #:use-module (rde features version-control)
  #:use-module (rde features documentation)
  #:use-module (rde features ssh)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features password-utils)
  #:export (shell-features))

(define (feature-bash)
  (define f-name 'bash)

  (define (get-home-services config)
    (list
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
                                               "\n"))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define (shell-features)
  (list
   (feature-bash)
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
    #:remote-password-store-url "git@github.com:j-shilling/password-store.git")))
