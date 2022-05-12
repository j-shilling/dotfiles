(define-module (home config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home-services state)
  #:use-module ((home services pass) #:prefix pass:)
  #:use-module ((home services gpg) #:prefix gpg:)
  #:use-module ((home services shells) #:prefix shells:)
  #:use-module ((home services emacs) #:prefix emacs:)
  #:use-module ((profiles default) #:prefix profile:)
  #:export (config))

(define state-service
  (service
   home-state-service-type
   (append pass:state
           emacs:state
           shells:state)))

(define config
  (home-environment
   (packages profile:default-packages)
   (services (append (list state-service)
                     pass:services
                     gpg:services
                     emacs:services
                     shells:services))))

config