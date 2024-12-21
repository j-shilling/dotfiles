(define-module (config development common-lisp)
  #:use-module (rde features lisp)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (nongnu packages emacs)
  #:export (common-lisp-features))

(define (common-lisp-features)
  (list
   (feature-lisp
    #:extra-lisp-packages
    (list
     clhs
     cl-asdf
     cl-quickproject
     cl-fiveam
     cl-trivia
     cl-alexandria
     cl-check-it))))
