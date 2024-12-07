(define-module (config tools mail)
  #:use-module (rde features mail)
  #:export (mail-features))

(define (mail-features)
  (list
   (feature-mail-settings
    #:mail-accounts
    (list
     (mail-account
      (id 'personal)
      (type 'gmail)
      (fqda "shilling.jake@gmail.com"))))
   (feature-isync)
   (feature-msmtp)
   (feature-emacs-message)
   (feature-emacs-org-mime)
   (feature-notmuch)))
