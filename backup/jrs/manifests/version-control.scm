(define-module (jrs manifests version-control)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("git"
         "nss-certs"
         "emacs-magit")))

(define manifest
  (packages->manifest packages))

manifest
