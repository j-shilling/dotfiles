(define-module (home services pass)
  #:use-module (gnu services)
  #:use-module (gnu home-services state)
  #:use-module (gnu home-services password-utils)
  #:export (services
            state))

(define pass-store-dir
   "$HOME/.local/var/lib/password-store")

(define state
  (list
   (state-git pass-store-dir
              "git@gitlab.com:shilling.jake/password-store.git")))

(define services
  (list
   (service
    home-password-store-service-type
    (home-password-store-configuration))))

services
