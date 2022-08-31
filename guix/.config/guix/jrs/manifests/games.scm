(define-module (jrs manifests games)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:use-module (jrs packages polymc)
  #:export (packages
            manifest))

(define packages
  (list polymc))

(define manifest
  (packages->manifest packages))

manifest
