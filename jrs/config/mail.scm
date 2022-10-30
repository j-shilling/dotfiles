(define-module (jrs config mail)
  #:use-module (rde features mail))

(define-public %mail-features
  (list
   (feature-emacs-message)
   (feature-isync
    #:isync-verbose #t)
   ;; (feature-l2md)
   (feature-notmuch)
   (feature-msmtp)))
