(define-module (config development terraform)
  #:use-module (rde features)
  #:export (terraform-features))

(define (feature-terraform)
  (define f-name 'terraform)

  (define (get-home-services config)
    (list))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define (terraform-features)
  (list))

(feature-terraform)
