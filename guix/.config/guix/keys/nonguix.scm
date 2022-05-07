(define-module (keys nonguix)
  #:use-module (guix gexp)
  #:export (nonguix))

(define nonguix
  (local-file "./nonguix.pub"))
