(define-module (config tools mail)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:export (mail-features))

(define (mail-features)
  (list
   (feature-mail-settings
    #:mail-accounts
    (list
     (mail-account
      (id 'personal)
      (type 'gmail)
      (fqda "shilling.jake@gmail.com")
      (pass-cmd "pass show mail/shilling.jake@gmail.com"))))
   (feature-irc-settings
    #:irc-accounts
    (list
     (irc-account
      (id 'libera)
      (network "irc.libera.chat")
      (bouncer? #f)
      (nick "Jacobissimus"))))
   (feature-emacs-erc
    #:erc-log? #t
    #:erc-kill-buffers-on-quit? #t
    #:erc-align-nicknames? #t
    #:erc-images? #t
    #:erc-autojoin-channels-alist
    '(("irc.libera.chat" "#guix" "#rde" "#emacs" "#emacs-beginners")))
   (feature-isync)
   (feature-l2md)
   (feature-msmtp)
   (feature-emacs-message)
   (feature-emacs-org-mime)
   (feature-notmuch)))
