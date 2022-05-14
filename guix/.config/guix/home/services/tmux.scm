(define-module (home services tmux)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home-services state)
  #:export (state
            services))

(define state
  (list
   (state-git "$HOME/.tmux/plugins/tpm"
              "https://github.com/tmux-plugins/tpm")))

(define services
  (list
   (simple-service 'tmux-file-service
                   home-files-service-type
                   (list
                    `("$HOME/.tmux.conf" ,(local-file "$HOME/dotfiles/tmux/.tmux.conf"))))))

services
