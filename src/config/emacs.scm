(define-module (config emacs)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde home services emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:export (emacs-features))

(define %extra-init-el
  '((use-pacakge emacs
                 :custom
                 (use-short-ansers t)
                 (ring-bell-function #'ignore))))

(define %extra-early-init-el '())
(define %additional-elisp-packages '())


(define* (emacs-features
          #:key
          (wayland? #f))
  (list
   (let ((emacs-pkg (if wayland?
                        emacs-pgtk
                        emacs)))
     (feature-emacs
      #:emacs emacs-pkg
      #:emacs-server-mode? #t
      #:default-terminal? #f
      #:default-application-launcher? #f))
   (feature
    (name 'emacs-custom)
    (home-services-getter
     (const
      (list
       (simple-service
        'emacs-extensions
        home-emacs-service-type
        (home-emacs-extension
         (init-el %extra-init-el)
         (early-init-el %extra-early-init-el)
         (elisp-packages %additional-elisp-packages)))))))
   (feature-emacs-appearance)
   (feature-emacs-modus-themes
    #:dark? #t)
   (feature-emacs-which-key)
   (feature-emacs-all-the-icons)
   (feature-emacs-tramp)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-completion)
   (feature-emacs-vertico)
   (feature-emacs-corfu)
   (feature-emacs-smartparens)
   (feature-emacs-eglot)
   (feature-emacs-flymake)
   (feature-emacs-git)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-xref)))
