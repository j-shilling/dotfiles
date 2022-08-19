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
         "unzip"
         "guile"
         "glibc-locales"
         "guix"
         "aspell"
         "aspell-dict-en"

         "emacs-next-pgtk"
         "emacs-paredit"
         "emacs-guix"
         "emacs-geiser"
         "emacs-pinentry"
         "emacs-helpful"

         ;; Completion
         "emacs-corfu"
         "emacs-cape"

         ;; Snippets
         "emacs-yasnippet"
         "emacs-yasnippet-snippets"

         ;; Dired
         "emacs-all-the-icons-dired"
         "emacs-dired-hacks"

         ;; Eshell
         "emacs-esh-autosuggest"
         "emacs-xterm-color"
         "emacs-exec-path-from-shell"
         "emacs-eshell-syntax-highlighting")))

(define manifest
  (packages->manifest packages))

manifest
