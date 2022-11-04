(define-module (jrs packages lpass)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) :prefix license:))

(define-public lpass
  (package
    (name "lpass")
    (version "1.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lastpass/lastpass-cli.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "168jg8kjbylfgalhicn0llbykd7kdc9id2989gg0nxlgmnvzl58a"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DCMAKE_C_FLAGS=-fcommon")
       #:tests? #f))
    (inputs
     (list pkg-config
           curl
           openssl
           bash-completion
           libxml2))
    (home-page "https://github.com/LastPass/lastpass-cli")
    (synopsis "LastPass command line interface")
    (description "LastPass command line interface")
    (license license:gpl2)))
