(define-module (jrs packages emacs-tree-sitter)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) :prefix license:))

(define-public emacs-tsc
  (package
    (name "emacs-tsc")
    (version "20220212.1632")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/emacs-tree-sitter/elisp-tree-sitter.git")
               (commit
                 "3cfab8a0e945db9b3df84437f27945746a43cc71")))
        (sha256
          (base32
            "0flqsf3nly7s261vss56havss13psgbw98612yj2xkfk9sydia28"))))
    (build-system emacs-build-system)
    (arguments
      '(#:files
        ("core/*.el"
         "core/Cargo.toml"
         "core/Cargo.lock"
         "core/src")))
    (home-page
      "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
    (synopsis "Core Tree-sitter APIs")
    (description
      "Documentation at https://melpa.org/#/tsc")
    (license #f)))

(define-public emacs-tree-sitter
  (package
   (name "emacs-tree-sitter")
   (version "0.18.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/emacs-tree-sitter/elisp-tree-sitter.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1sdvz827v436qijs6xafakkfw2d16bvp8frymd818rppjc7a9dif"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
   (propagated-inputs (list emacs-tsc))
   (synopsis "Tree-sitter bindings for Emacs Lisp")
   (description
    "tree-sitter is an Emacs binding for Tree-sitter, an incremental parsing system.

It aims to be the foundation for a new breed of Emacs packages that
understand code structurally. For example:

  -  Faster, fine-grained code highlighting.
  -  More flexible code folding.
  -  Structural editing (like Paredit, or even better) for non-Lisp code.
  -  More informative indexing for imenu.")
   (license license:expat)))

emacs-tree-sitter
