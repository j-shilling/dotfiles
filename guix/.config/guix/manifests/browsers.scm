(define-module (manifests browsers)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages gnuzilla)
  #:use-module (guix profiles)
  #:export (browsers-packages
            browsers-manifest))

(define browsers-packages
  (list nyxt
        icecat))

(define browsers-manifest
  (packages->manifest browsers-packages))

browsers-manifest
