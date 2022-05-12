(define-module (manifests browsers)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages gnuzilla)
  #:use-module (guix profiles)
  #:export (browsers-packages
            browsers-manifest))

(when (resolve-module '(nongnu packages mozilla) #:ensure #f)
  (use-modules (nongnu packages mozilla)))

(define browsers-packages
  (list nyxt
        (if (defined? 'firefox)
            firefox
            icecat)))

(define browsers-manifest
  (packages->manifest browsers-packages))

browsers-manifest
