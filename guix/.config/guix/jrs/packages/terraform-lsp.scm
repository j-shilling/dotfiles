(define-module (jrs packages terraform-lsp)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:))

(define-public terraform-lsp
  (package
   (name "terraform-lsp")
   (version "0.0.12")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/juliosueiras/terraform-lsp.git")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "111350jbq0dp0qhk48j12hrlisd1fwzqpcv357igrbqf6ki7r78q"))))
   (home-page "https://github.com/juliosueiras/terraform-lsp#building")
   (synopsis "Language Server Protocol for Terraform")
   (description "")
   (build-system go-build-system)
   (arguments
     '(#:import-path "github.com/juliosueiras/terraform-lsp"))
   (license license:expat)))

terraform-lsp
