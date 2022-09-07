(define-module (jrs manifests emacs)
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
         "emacs-flyspell-correct"

         "emacs-next-pgtk"
         "emacs-paredit"
         "emacs-guix"
         "emacs-geiser"
         "emacs-pinentry"
         "emacs-helpful"

         ;; Completion
         "emacs-corfu"
         "emacs-cape"
         "emacs-vertico"
         "emacs-consult"
         "emacs-consult-yasnippet"
         "emacs-marginalia"
         "emacs-embark"

         ;; Snippets
         "emacs-yasnippet"
         "emacs-yasnippet-snippets"

         ;; Syntax Highlighting
         "tree-sitter"
         ;; TODO: Figure out emacs-tree-sitter

         ;; Dired
         "emacs-all-the-icons-dired"

         ;; Eshell
         "emacs-esh-autosuggest"
         "emacs-xterm-color"
         "emacs-exec-path-from-shell"
         "emacs-eshell-syntax-highlighting")))

(define manifest
  (packages->manifest packages))

manifest
