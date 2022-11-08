(define-module (jrs config users)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features mail)
  #:use-module (rde features password-utils))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define-public %user-features
  (list
   (feature-user-info
    #:user-name "jake"
    #:full-name "Jake Shilling"
    #:email "shilling.jake@gmail.com"
    #:user-groups '("wheel" "netdev" "audio" "lp" "video" "tty" "kvm")
    #:user-initial-password-hash
    "$6$wBQv6IQzjX$fKWemlD4YAIEcK4IgHzn68VNXyTNSL/MCVeiRH05cPHDyQLSse.k57ZPndZk83ddtnYBglH7GAH4E65nnk4y4/"
    #:emacs-advanced-user? #t
    #:rde-advanced-user? #f)

   (feature-mail-settings
    #:mail-accounts (list (mail-account
                           (id 'personal)
                           (fqda "shilling.jake@gmail.com")
                           (type 'gmail))
                          (mail-account
                           (id 'work)
                           (fqda "jshilling@arena.io")
                           (type 'gmail)))
    #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1"))))

   (feature-gnupg
    #:gpg-primary-key "0FCC8E6A96FF109F"
    ;; #:pinentry-flavor 'emacs
    #:ssh-keys
    '(("6B626401361800A59771759B5FBF44151969E206")
      ("E556265A9520AFE6C5BEC85C47B1ADB883CCBC91")))

   (feature-password-store
    #:remote-password-store-url "git@gitlab.com:shilling.jake/password-store.git")))
