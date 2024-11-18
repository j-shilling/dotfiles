(define-module (config home services gpg)
  #:use-module (guix gexp)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:export (gpg-services))

(define gpg-packages
  (list gnupg pinentry-emacs))

(define gpg-packages-service
  (simple-service 'readline-service
                  home-profile-service-type
                  gpg-packages))

(define gpg-agent-service
  (service home-gpg-agent-service-type
           (home-gpg-agent-configuration
            (pinentry-program
             (file-append pinentry-emacs "/bin/pinentry-emacs"))
            (ssh-support? #t)
            (extra-content "allow-loopback-entry\nallow-emacs-pinentry"))))

(define gpg-services
  (list
   gpg-packages-service
   gpg-agent-service))
