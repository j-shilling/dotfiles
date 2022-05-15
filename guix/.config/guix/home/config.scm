(define-module (home config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home-services state)
  #:use-module ((home services pass) #:prefix pass:)
  #:use-module ((home services gpg) #:prefix gpg:)
  #:use-module ((home services shells) #:prefix shells:)
  #:use-module ((home services emacs) #:prefix emacs:)
  #:use-module ((home services tmux) #:prefix tmux:)
  #:use-module ((profiles default) #:prefix default:)
  #:export (config))

(define state-service
  (service
   home-state-service-type
   (append pass:state
           emacs:state
           shells:state
           tmux:state)))

(define config
  (home-environment
   (packages default:packages)
   (services (append (list state-service)
                     pass:services
                     gpg:services
                     emacs:services
                     shells:services
                     tmux:services))))

config
