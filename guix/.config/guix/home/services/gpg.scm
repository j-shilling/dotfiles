(define-module (home services gpg)
  #:use-module (gnu services)
  #:use-module (gnu home-services gnupg)
  #:export (services))

(define services
  (list
   (service home-gnupg-service-type
            (home-gnupg-configuration
             (gpg-agent-config
              (home-gpg-agent-configuration
               (ssh-agent? #t)
               (ssh-keys
                (ssh-keys-list
                 '(("57CCEEB098F2AA6791BA6D8F4CEF32B3F147C678")
                   ("BA4555615BAE014981589CE5FA8A56E69156CDBE")
                   ("6B626401361800A59771759B5FBF44151969E206"))))))))))

services
