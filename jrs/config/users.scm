(define-module (jrs config users)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features mail)
  #:use-module (rde features password-utils))

(define-public %user-features
  (list
   (feature-user-info
    #:user-name "jake"
    #:full-name "Jake Shilling"
    #:email "shilling.jake@gmail.com"
    #:user-groups '("wheel" "netdev" "audio" "lp" "video" "tty")
    #:user-initial-password-hash #f
    #:emacs-advanced-user? #t
    #:rde-advanced-user? #f)

   (feature-mail-settings
    #:mail-accounts (list (mail-account
                           (id 'personal)
                           (fqda "shilling.jake@gmail.com")
                           (type 'gmail)))
    #:mailing-lists '())

   (feature-gnupg
    #:gpg-primary-key "0FCC8E6A96FF109F"
    #:pinentry-flavor 'emacs
    #:ssh-keys
    '(("57CCEEB098F2AA6791BA6D8F4CEF32B3F147C678")
      ("E556265A9520AFE6C5BEC85C47B1ADB883CCBC91")
      ("57407F876080A07BE7455903C9F52FB2922C8C49")))

   (feature-password-store
    #:remote-password-store-url "git@gitlab.com:shilling.jake/password-store.git")))
