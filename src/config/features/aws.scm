(define-module (config features aws)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages python-web)
  #:use-module (guix gexp)
  #:use-module (rde serializers ini)
  #:export (feature-aws))

(define (feature-aws)
  (define f-name 'aws)

  (define (get-home-services config)
    (define (add-aws-config-files config)
      `((".aws-config" ,(mixed-text-file "foo"))))

    (define pass "pass")

    (define (pass-show v)
      (string->symbol (string-join (list "pass" "show" v))))

    (list
     (simple-service
      'add-aws-packages
      home-profile-service-type
      (list
       awscli-2))
     (simple-service
      'add-aws-config-files
      home-files-service-type
      `((".aws/config"
         ,(mixed-text-file
           "aws-config"
           "\
[default]
region = us-east-1
output = json

[profile seeltest]
region = us-east-1
output = json

[profile seeldeals]
region = us-east-1
output = json\n"))
        (".aws/credentials"
         ,(mixed-text-file
           "aws-config"
           "\
[default]
credential_process = " pass " show FunctorFactory/aws

[seeltest]
credential_process = " pass " show FunctorFactory/SeelTheDeal/aws-seeltest

[seeldeals]
credential_process = " pass " show FunctorFactory/SeelTheDeal/aws\n"))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))
