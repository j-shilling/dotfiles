(define-module (jrs manifests browsers)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:use-module (nongnu packages mozilla)
  #:export (packages
            manifest))

(define packages
  (append
   (map (compose list specification->package+output)
        '("nyxt"))
   `((,firefox "out"))))

(define manifest
  (packages->manifest packages))

manifest
