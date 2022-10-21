(define-module (jrs config shell)
  #:use-module (rde features tmux)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features xdg))

(define-public %shell-features
  (list
   (feature-tmux)
   (feature-bash)
   (feature-direnv)
   (feature-ssh)
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)

   (feature-xdg)))
