(define-module (config tools mail)
  #:use-module (gnu services)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:export (mail-features))

(define* (feature-mail-mcron
          #:key
          (time-spec '(next-minute
                       (range 0 60 5))))
  "Configure mcron to invoke other email commands based on the other
features that have been enabled."
  (define f-name 'mail-mcron)

  (define (get-home-services config)
    (list
     (when (get-value 'isync config #f)
       (let* ((mbsync (get-value 'mbsync config))
              (mail-acc->isync-args (get-value 'mail-acc->isync-args config))
              (mail-accounts
               (filter (lambda (x) (eq? (mail-account-synchronizer x) 'isync))
                       (get-value 'mail-accounts config)))
              (sync-cmds (map (lambda (acc) #~(#$(mbsync config) (mail-acc->isync-args acc)))
                              mail-accounts))
              (notmuch-cmds (if (get-value 'notmuch config #f)
                                (list "notmuch new")
                                (list)))
              (l2md-cmds (if (get-value 'l2md config #f)
                             (list "l2md")
                             (list))))
         (let ((cmds (append l2md-cmds sync-cmds notmuch-cmds)))
           (simple-service
            'isync-mcron-job
            home-mcron-service-type
            (list
             #~(job '#$time-spec
                    '(for-each system '#$cmds)))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

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
   (feature-mail-mcron)
   (feature-msmtp)
   (feature-emacs-message)
   (feature-emacs-org-mime)
   (feature-notmuch)))
