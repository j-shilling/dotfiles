(define-module (jrs manifests lisp)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define specifications
  '("emacs-slime"
    "sbcl"))

(define packages
  (map (compose list specification->package+output)
       specifications))

(define manifest
  (packages->manifest packages))

manifest
