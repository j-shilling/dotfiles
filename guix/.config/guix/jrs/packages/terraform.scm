(define-module (jrs packages terraform)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) :prefix license:))

(define-public terraform
  (package
   (name "terraform")
   (version "1.2.8")
   (source (origin (method url-fetch)
                   (uri "https://releases.hashicorp.com/terraform/1.2.8/terraform_1.2.8_linux_amd64.zip")
                   (sha256
                    (base32
                     "18p2mgwrkzrsb5wrc92myfqgzc31k7c5dh8qa06yjf3kygb4d71y"))))
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
