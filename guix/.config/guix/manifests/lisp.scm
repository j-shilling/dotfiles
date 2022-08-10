(define-module (manifests lisp)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("emacs-sly"
         "emacs-sly-stepper"
         "emacs-sly-quicklisp"
         "emacs-sly-macrostep"
         "emacs-sly-asdf"
         "sbcl")))

(define manifest
  (packages->manifest packages))

manifest
