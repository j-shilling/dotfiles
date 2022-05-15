(define-module (home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services state)
  #:export (services
            state))

(define state
  (list (state-git "$HOME/.doom.d"
                   "git@gitlab.com:dotfiles11/doom.d.git")
        (state-git "$HOME/.emacs.d"
                   "https://github.com/doomemacs/doomemacs")))

(define services
  (list
   (service
    home-emacs-service-type
    (home-emacs-configuration
     (package emacs)
     (rebuild-elisp-packages? #f)
     (server-mode? #t)))))

services
