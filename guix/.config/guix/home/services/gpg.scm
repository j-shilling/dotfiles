(define-module (home services gpg)
  #:use-module (gnu services)
  #:use-module (gnu home-services gnupg)
  #:export (services))

(define services
  (list
   (service home-gnupg-service-type
            (home-gnupg-configuration
             (gpg-agent-config
              (home-gpg-agent-configuration
               (ssh-agent? #t)))))))

services
