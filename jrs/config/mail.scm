(define-module (jrs config mail)
  #:use-module (jrs features mail)
  #:use-module (rde features mail))

(define-public %mail-features
  (list
   (feature-emacs-message)
   (feature-isync
    #:isync-verbose #t)
   (feature-l2md-no-mcron)
   (feature-notmuch)
   (feature-afew)
   (feature-msmtp)))
