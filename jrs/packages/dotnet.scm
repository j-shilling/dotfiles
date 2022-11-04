(define-module (jrs packages dotnet)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy))

(define-public dotnet
  (package
    (name "dotnet")
    (version "6.0.402")
    (source (origin (method url-fetch)
                    (uri
                     "https://download.visualstudio.microsoft.com/download/pr/d3e46476-4494-41b7-a628-c517794c5a6a/6066215f6c0a18b070e8e6e8b715de0b/dotnet-sdk-6.0.402-linux-x64.tar.gz")
                    (sha256
                     (base32
                      "11gflfydd5b7lkmsczrxaaxhh69077rn4w31qrnzfx0h5c42apv3"))))
    (home-page "dotnet.microsoft.com")
    (synopsis "dotnet")
    (description "")
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(())))))
