(define-module (config tools syncthing)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu home services syncthing)
  #:export (syncthing-features))

(define (feature-syncthing)
  (define f-name 'syncthing)

  (define (get-home-services config)
    (list
     (service home-syncthing-service-type
              (for-home
               (syncthing-configuration)))
     (simple-service
      'add-syncthing-to-profile
      home-profile-service-type
      (list
       syncthing
       syncthing-gtk))))

  (feature
   (name f-name)
   (values `((,f-name #t)))
   (home-services-getter get-home-services)))

(define (syncthing-features)
  (list
   (feature-syncthing)))
