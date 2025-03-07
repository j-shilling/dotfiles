(define-module (home)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services ssh)
  #:use-module (gnu services)
  #:use-module (rde features)
  #:use-module (rde-config))

(define-public he
  (rde-config-home-environment config))

he
