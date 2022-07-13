(define-module (manifests browsers)
  #:use-module (gnu packages web-browsers)
  #:use-module (nongnu packages mozilla)
  #:use-module (guix profiles)
  #:export (browsers-packages
            browsers-manifest))

(define browsers-packages
  (list nyxt
        firefox))

(define browsers-manifest
  (packages->manifest browsers-packages))

browsers-manifest
