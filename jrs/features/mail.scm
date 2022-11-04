(define-module (jrs features mail)
  #:use-module (ice-9 regex)

  #:use-module (gnu packages mail)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)

  #:use-module (guix gexp)

  #:use-module (rde features)
  #:use-module (rde features mail)
  #:use-module (rde serializers ini)
  #:export (feature-l2md-no-mcron
            feature-afew))

(define* (feature-l2md-no-mcron)
  (let ((rde-feature (feature-l2md)))

    (define (get-home-services config)
      (let ((orig-fn (feature-home-services-getter rde-feature)))
        (modify-services (orig-fn config)
          (delete home-mcron-service-type))))

    (feature
     (name (feature-name rde-feature))
     (values (feature-values rde-feature))
     (home-services-getter get-home-services))))

(define* (feature-afew)
  "configure afew."

  (define (get-home-services config)
    (list
     (simple-service
      'notmuch-homedir-config
      home-files-service-type
      `((".notmuch-config"
         ,(apply
           mixed-text-file
           "notmuch-home-config"
           (serialize-ini-config
            `((new ((tags . new)))))))))

     (let* ((mail-accounts (get-value 'mail-accounts config))
            (folders (map (lambda (acc)
                            (string-append "accounts/"
                                           (mail-account-fqda acc)
                                           "/inbox"))
                          mail-accounts))
            (archive-rules (map (lambda (inbox)
                                  (let ((dest
                                         (regexp-substitute
                                          #f
                                          (string-match
                                           "inbox" inbox)
                                          'pre "archive" 'post)))
                                    (cons (string->symbol inbox)
                                          (string->symbol
                                           (format #f "'NOT tag:inbox':~a" dest)))))
                                folders)))

       (simple-service
        'afew-config
        home-xdg-configuration-files-service-type
        `(("afew/config"
           ,(apply
             mixed-text-file
             "afew-config"
             (serialize-ini-config
              `((MailMover ((folders . ,(string->symbol (string-join folders " ")))
                            (rename . False)
                            ,@archive-rules)))))))))
     (simple-service
      'afew-package
      home-profile-service-type
      (list afew))))

  (feature
   (name 'afew)
   (home-services-getter get-home-services)))
