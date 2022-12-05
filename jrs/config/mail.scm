(define-module (jrs config mail)
  #:use-module (rde features mail)
  #:use-module (guix gexp))

(define (tag-update-for-mail-list tag addr)
  (format #f "notmuch tag +~a -- '(to:~a or from:~a) and tag:new'"
          tag addr addr))

(define more-tag-updates
  (map (lambda (args) (apply tag-update-for-mail-list args))
       '(("emacs-devel" "emacs-devel@gnu.org")
         ("emacs-user" "help-gnu-emacs@gnu.org")
         ("rde-discuss" "~abcdw/rde-devel@lists.sr.ht")
         ("rde-devel" "~abcdw/rde-devel@lists.sr.ht")
         ("guix-bugs" "bug-guix@gnu.org")
         ("guix-devel" "guix-devel@gnu.org")
         ("guix-patches" "guix-patches@gnu.org")
         ("guix-user" "help-guix@gnu.org"))))

(define-public %mail-features
  (list
   (feature-emacs-message)
   (feature-isync
    #:isync-verbose #t)
   (feature-notmuch
    #:extra-tag-updates-post
    more-tag-updates)
   (feature-msmtp)
   (feature-mail-mcron)))
