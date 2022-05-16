(define-module (home services shells)
  #:use-module (gnu services)
  #:use-module (gnu home-services state)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils)
  #:export (services
            state))

(define state
  (list
   (state-git "$HOME/dotfiles/"
              "git@gitlab.com:shilling.jake/dotfiles.git")
   (state-git "$HOME/.oh-my-zsh/"
              "https://github.com/ohmyzsh/ohmyzsh.git")))

(define services
  (list
   (service
    home-bash-service-type
    (home-bash-configuration
     (guix-defaults? #t)
     (bash-profile
      '("source $HOME/dotfiles/shell/.bash_profile"))
     (bashrc
      '("source $HOME/dotfiles/shell/.bashrc"))))
   (service
    home-zsh-service-type
    (home-zsh-configuration
     (zshrc
      '("source $HOME/dotfiles/shell/.zshrc"))))
   (service home-bash-direnv-service-type)
   (service home-zsh-direnv-service-type)
   (service home-zsh-autosuggestions-service-type)))
