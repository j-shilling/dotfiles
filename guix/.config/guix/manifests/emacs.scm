(define-module (manifests emacs)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("coreutils"
         "bash"
         "gzip"
         "guile"
         "glibc-locales"
         "guix"
         "emacs-next-pgtk"
         "emacs-paredit"
         "emacs-guix"
         "emacs-geiser"
         "emacs-pinentry"
         "emacs-corfu"
         "emacs-cape"
         "emacs-yasnippet"
         "emacs-yasnippet-snippets")))

(define manifest
  (packages->manifest packages))

manifest
