(define-module (config features guix)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home services guix)
  #:export (feature-guix))

(define* (feature-guix #:key (profile #f))
  (define f-name 'guix)
  (define channels (if profile
                       (profile-channels profile)
                       '()))

  (define (get-home-services config)
    `(`@(if (null-list? channels)
            (simple-service 'set-guix-channels
                            home-channels-service-type
                            channels)
            (list))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (guix-channels . ,channels)))))
