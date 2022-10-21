(define-module (jrs config mail)
  #:use-module (rde features mail))

(define-public %mail-features
  (list
   (feature-isync)
   (feature-notmuch)
   (feature-msmtp)))
