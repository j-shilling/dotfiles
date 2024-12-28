(define-module (config development guile)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features guile)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde home services emacs)
  #:use-module (rde serializers lisp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:export (guile-features))

(define (feature-emacs-guile-config)
  (define emacs-f-name 'guile-config)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (simple-service
      'add-guile-packages
      home-profile-service-type
      (list
       guile-next
       guile-readline
       guile-colorized))
     (simple-service
      'add-guile-config
      home-files-service-type
      `((".guile"
         ,(mixed-text-file "guile"
           (sexp-serialize
           '((use-modules (ice-9 readline)
                          (ice-9 colorized))
             (activate-readline)
             (activate-colorized)))))))
     (simple-service
        'emacs-extensions
        home-emacs-service-type
        (home-emacs-extension
         (elisp-packages (list emacs-geiser-latest
                               emacs-geiser-guile-latest
                               emacs-guix-latest))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define (guile-features)
  (list
   (feature-guile)
   (feature-emacs-guile-config)))
