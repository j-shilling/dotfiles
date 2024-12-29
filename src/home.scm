(define-module (home)
  #:use-module (rde features)
  #:use-module (rde-config))

(define-public he
  (rde-config-home-environment config))

he
