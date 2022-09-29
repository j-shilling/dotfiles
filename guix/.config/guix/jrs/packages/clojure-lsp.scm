(define-module (jrs packages clojure-lsp)
  #:use-module (srfi srfi-28)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) :prefix license:))

(define-public clojure-lsp
  (package
   (name "clojure-lsp")
   (version "2022.09.01-15.27.31")
   (source (origin (method url-fetch)
                   (uri
                    (format
                     "https://github.com/clojure-lsp/clojure-lsp/releases/download/~a/clojure-lsp-native-linux-amd64.zip"
                     version))
                   (sha256
                    (base32
                     "1sj6mlv5i124rksyyyd52pvcr136jb73wzv04zx3hmik9kvznbl0"))))
   (home-page "https://www.clojure-lsp.io")
   (synopsis "Clojure LSP")
   (description "")
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("./clojure-lsp" "bin/"))))
   (inputs (list unzip))
   (license license:expat)))

clojure-lsp
