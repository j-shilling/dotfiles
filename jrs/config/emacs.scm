(define-module (jrs config emacs)
  #:use-module (gnu packages)

  #:use-module (guix gexp)

  #:use-module (rde gexp)

  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features terminals)

  #:use-module (jrs features emacs-xyz))

(define-public %emacs-features
  (list
   (feature-emacs
    #:additional-elisp-packages
    (map specification->package+output
         '("emacs-paredit"
           "emacs-pinentry"))
    #:extra-init-el
    `(,(slurp-file-like (local-file "../../elisp/configure-defaults.el"))
      ,(slurp-file-like (local-file "../../elisp/configure-lisp.el"))))

   (feature-emacs-appearance
    #:dark? #t)
   (feature-emacs-faces)
   (feature-emacs-tramp)
   (feature-emacs-completion)
   (feature-emacs-corfu)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-which-key)
   (feature-emacs-keycast
    #:turn-on? #f)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-vterm)
   (feature-emacs-git)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-eglot)

   (feature-emacs-pdf-tools)
   (feature-emacs-org)
   (feature-emacs-org-agenda)))
