(define-module (manifests lisp)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("emacs-slime"
         "sbcl")))

(define manifest
  (packages->manifest packages))

manifest
