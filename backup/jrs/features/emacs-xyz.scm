(define-module (jrs features emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)

  #:export (feature-emacs-magit-forge))

(define (feature-emacs-magit-forge)
  (define (home-magit-forge-services config)
    (list
     (rde-elisp-configuration-service
      'magit-forge
      config
      `((with-eval-after-load
         'magit
         (require 'forge)))
      #:keywords '(git version-control)
      #:summary "\
Adds and loads magit-forge."
      #:commentary "\
Forge adds integration with Github/Gitlab/etc. to magit."
      #:elisp-packages
      (list emacs-forge))))

  (feature
   (name 'magit-forge)
   (home-services-getter home-magit-forge-services)))
