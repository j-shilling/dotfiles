(define-module (jrs features javascript)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)

  #:use-module (gnu packages node)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:export (feature-javascript))

(define* (feature-javascript
          #:key
          (node-pkg node-lts))
  "Setup and configure environment for JavaScript/TypeScript."
  (ensure-pred any-package? node)

  (define (get-home-services config)
    (let ((emacs-f-name 'javascript))
      (list
       (when (get-value 'emacs config)
         (rde-elisp-configuration-service
          emacs-f-name
          config
          '((dolist (hook '(js-mode-hook
                            typescript-mode-hook))
                    (add-hook hook
                              (lambda ()
                                (setq tab-width 2)))
                    (add-hook hook 'eglot)))
          #:summary "\
JavaScript/TypeScript configuration and pacakges."
          #:commentary "\
Configuration for JavaScript/TypeScript development with node and LSP."
          #:keywords '(javascript typescript)
          #:elisp-packages
          (list emacs-typescript-mode
                emacs-nodejs-repl
                (get-value 'emacs-eglot config emacs-eglot))))

       (simple-service
        'packages-for-javascript
        home-profile-service-type
        (list
         node-pkg)))))

  (feature
   (name 'javascript)
   (values `((javascript . #t)
             (typescript . #t)))
   (home-services-getter get-home-services)))
