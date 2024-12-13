(define-module (config packages terraform)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix download))

(define-public terraform
  (package
   (name "terraform")
   (version "1.10.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://releases.hashicorp.com/terraform/" version "/terraform_" version "_linux_amd64.zip"))
            (sha256
             (base32
              "1i181cmzwlrx8d40z1spilcwgnhkzwalrg8822d23sqdmrs7a5hj"))))
   ;; (build-system binary-build-system)
   (supported-systems '("x86_64-linux"))
   (synopsis "A tool to describe and deploy infrastructure as code")
   (description
    "Terraform allows you to describe your complete infrastructure in the
form of code. Even if your servers come from different providers such
as AWS or Azure, Terraform helps you build and manage these resources
in parallel across providers.")
   (home-page "https://hashicorp.com/terraform")
   (license #f)))

terraform
