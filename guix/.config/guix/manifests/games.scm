(define-module (manifests games)
  #:use-module (guix profiles)
  #:use-module (packages multimc)
  #:export (packages
            manifest))

(define packages
  (list multimc))

(define manifest
  (packages->manifest packages))

manifest
