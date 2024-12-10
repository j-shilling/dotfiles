(define-module (config development python)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages file)
  #:use-module (rde features)
  #:use-module (rde features python)
  #:use-module (rde home services emacs)
  #:export (python-features))

(define (extra-python-feature)
  (define f-name 'extra-python)

  (define (get-home-services config)
    (list
     (simple-service
      'add-python-tree-sitter
      home-profile-service-type
      (list tree-sitter-python
            poetry
            file))
     (simple-service
      'emacs-extensions
      home-emacs-service-type
      (home-emacs-extension
       (elisp-packages
        (list
         emacs-pyvenv))
       (init-el
        `((eval-when-compile (require 'use-package))
          (use-package python-ts-mode
                       :mode "\\.py[iw]?\\'"
                       :interpreter ,(file-append python-3.12 "/bin/python3.12")
                       :hook (python-ts-mode . eglot-ensure))
          (use-package eglot
                       :config
                       (add-to-list 'eglot-server-programs
                                    (cons '(python-mode python-ts-mode)
                                          (eglot-alternatives
                                           '(("basedpyright-langserver" "--stdio")
                                             "pylsp"
                                             "pyls"
                                             ("pyright-langserver" "--stdio")
                                             "jedi-language-server"
                                             "ruff-lsp")))))))))))

  (feature
   (name f-name)
   (home-services-getter get-home-services)))

(define (python-features)
  (list
   (feature-python
    #:python python-3.12
    #:black? #t)
   (extra-python-feature)))
