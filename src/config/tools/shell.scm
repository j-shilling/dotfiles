(define-module (config tools shell)
  #:use-module (rde features gnupg)
  #:use-module (rde features version-control)
  #:use-module (rde features documentation)
  #:use-module (rde features ssh)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features password-utils)
  #:export (shell-features))

(define (shell-features)
  (list
   (feature-bash)
   (feature-manpages)
   (feature-direnv)
   (feature-gnupg
    #:gpg-primary-key "0FCC8E6A96FF109F"
    #:ssh-keys
    '(("E556265A9520AFE6C5BEC85C47B1ADB883CCBC91")))
   (feature-ssh)
   (feature-git
    #:sign-commits? #t)
   (feature-password-store
    #:remote-password-store-url "git@github.com:j-shilling/password-store.git")))
