(define-module (config development common-lisp)
  #:use-module (rde features lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:export (common-lisp-features))

(define (common-lisp-features)
  (list
   (feature-lisp
    #:extra-lisp-packages
    (list
     sbcl-quickproject
     sbcl-fiveam
     sbcl-trivia
     sbcl-alexandria))))
