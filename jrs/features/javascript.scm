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
       (rde-elisp-configuration-service
        emacs-f-name
        config
        '((add-hook 'js-mode-hook 'npm-mode)

          (add-hook 'js2-mode-hook 'js2-refactor-mode)
          (with-eval-after-load
           'js2-mode
           (setq js-chain-indent t
                 js2-basic-offset 2
                 js2-skip-preprocessor-directives t
                 js2-mode-show-parse-errors t
                 js2-mode-show-strict-warnings t
                 js2-strict-missing-semi-warning t
                 js2-highlight-level 3
                 js2-idle-timer-delay 0.15))

          (add-hook 'rjsx-mode-hook 'js2-refactor-mode)
          (add-hook 'jrsx-mode-hook 'rainbow-delimiters-mode)
          (add-to-list 'auto-mode-alist '("\\.[mc]?js\\'" . rjsx-mode))
          (add-to-list 'auto-mode-alist '("\\.es6\\'" . rjsx-mode))
          (add-to-list 'auto-mode-alist '("\\.pac\\'" . rjsx-mode))
          (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))

          (with-eval-after-load
           'jrsx-mode
           (setq js-switch-indent-offset js2-basic-offset))

          (add-hook 'typescript-mode 'rainbow-delimiters-mode)
          (add-hook 'typescript-mode 'npm-mode)
          (add-hook 'typescript-tsx-mode 'rainbow-delimiters-mode)
          (with-eval-after-load
           'typescript-mode
           (setq typescript-auto-indent-flag t
                 typescript-autoconvert-to-template-flag t
                 typescript-indent-level 2
                 typescript-indent-list-items t
                 typescript-ident-switch-clauses 2))

          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
          (autoload 'typescript-tsx-mode "typescript-mode" nil t)
          (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX")

          (dolist (hook '(js2-mode-hook
                          typescript-mode-hook
                          typescript-tsx-mode-hook
                          web-mode-hook
                          rjsx-mode-hook))
                  (add-hook hook
                            (lambda ()
                              (setq tab-width js2-basic-offset)))))
        #:summary "\
JavaScript/TypeScript tools and pacakges."
        #:commentary "\
Configure tools for JavaScript development."
        #:keywords '(javascript typescript)
        #:elisp-packages
        (list emacs-js2-mode
              emacs-js2-refactor-el
              emacs-rjsx-mode
              emacs-typescript-mode
              emacs-web-mode
              emacs-rainbow-delimiters
              emacs-nodejs-repl
              emacs-npm-mode
              emacs-skewer-mode
              (get-value 'emacs-eglot config emacs-eglot)))
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
