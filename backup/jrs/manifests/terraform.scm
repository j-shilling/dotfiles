(define-module (jrs manifests terraform)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:use-module (jrs packages terraform)
  #:use-module (jrs packages terraform-lsp)
  #:use-module (jrs packages kubectl)
  #:export (packages
            manifest))

(define packages
  (append
   (map (compose list specification->package+output)
        '("emacs-eglot"
          "emacs-terraform-mode"))
   ;; TODO: Figure out how to get tree-sitter working and use:
   ;; https://github.com/mandos/tree-sitter-terraform
   `((,terraform "out")
     (,terraform-lsp "out")
     (,kubectl "out"))))

(define manifest
  (packages->manifest packages))

manifest
