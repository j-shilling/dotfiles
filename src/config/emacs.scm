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

(define (feature-emacs-base-config)
  (define emacs-f-name 'base-config)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (simple-service
        'emacs-extensions
        home-emacs-service-type
        (home-emacs-extension
         (init-el
          `((eval-when-compile
             (require 'use-package))
            (use-package emacs
                         :custom
                         (user-full-name ,(get-value 'full-name config))
                         (user-mail-address ,(get-value 'email config)))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

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
   (feature-emacs-base-config)
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
   (feature-emacs-project)
   (feature-emacs-ace-window)
   (feature-emacs-smartparens)
   (feature-emacs-eglot)
   (feature-emacs-dape)
   (feature-emacs-flymake)
   (feature-emacs-git)
   (feature-emacs-guix)
   (feature-emacs-xref)
   (feature-emacs-pdf-tools)
   (feature-emacs-help)
   (feature-emacs-info)
   (feature-emacs-devdocs)
   (feature-emacs-org)
   (feature-emacs-org-roam
    #:org-roam-directory "~/org/roam"
    #:org-roam-todo? #t)
   (feature-emacs-citation)
   (feature-emacs-spelling)))
