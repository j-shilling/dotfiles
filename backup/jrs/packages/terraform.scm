(define-module (jrs packages terraform)
  #:use-module (srfi srfi-28)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) :prefix license:))

(define-public terraform
  (package
   (name "terraform")
   (version "1.2.9")
   (source (origin (method url-fetch)
                   (uri
                    (format
                     "https://releases.hashicorp.com/terraform/~a/terraform_~a_linux_amd64.zip"
                     version version))
                   (sha256
                    (base32
                     "1yzjcbis9syfz17sykg7j157dbdgm59ijbhj0dqw3nmd863c63qf"))))
   (home-page "https://www.terraform.io")
   (synopsis "Terraform")
   (description "")
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("./terraform" "bin/"))))
   (inputs (list unzip))
   (license license:expat)))

terraform
