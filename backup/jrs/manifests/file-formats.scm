(define-module (jrs manifests file-formats)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("emacs-yaml-mode"
         "emacs-eglot"
         "node" ;; for yaml-lsp
         )))

(define manifest
  (packages->manifest packages))

manifest
