(define-module (config tools aws)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages python-web)
  #:export (aws-features))

(define (feature-aws)
  (define f-name 'aws)

  (define (get-home-services config)
    (list
     (simple-service
      'add-aws-packages
      home-profile-service-type
      (list
       awscli-2))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define (aws-features)
  (list
   (feature-aws)))
