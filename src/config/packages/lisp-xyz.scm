(define-module (config package lisp-xyz)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check))

(define-public sbcl-quicklisp-bootstrap
  (let ((commit "290a126c89839e79261438ac0a069db76ebe2432")
        (revision "0"))
    (package
      (name "sbcl-quicklisp-bootstrap")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/quicklisp/quicklisp-bootstrap")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lpqkkhz8k3bs22rbqnpjh9r77jjcpamrkiw5dw9dilap75xsr2g"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list))
      (home-page "www.quicklisp.org/")
      (synopsis "Quicklisp bootstrap")
      (description "")
      (license license:expat))))

(define-public sbcl-quicklisp
  (let ((commit "8b63e00b3a2b3f96e24c113d7601dd03a128ce94")
        (revision "0"))
    (package
      (name "sbcl-quicklisp")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/quicklisp/quicklisp-client")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "102f1chpx12h5dcf659a9kzifgfjc482ylf73fg1cs3w34zdawnl"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list))
      (home-page "www.quicklisp.org/")
      (synopsis "Quicklisp client")
      (description "")
      (license license:expat))))

(define-public sbcl-fuzzy-match
  (let ((commit "e46ca41ef4641461f7be006782e3cfdcf73ba98")
        (revision "0"))
    (package
      (name "sbcl-fuzzy-match")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vindarel/fuzzy-match")
               (commit commit)))
         (file-name (git-file-name "cl-fuzzy-match" version))
         (sha256
          (base32 "1lawndmzkl6f9sviy7ngn2s3xkc4akp8l505kvpslaz6qq0ayyqv"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list
        sbcl-cl-str
        sbcl-mk-string-metrics))
      (home-page "https://github.com/vindarel/fuzzy-match")
      (synopsis "Fuzzy match candidates from an input string.")
      (description "")
      (license license:expat))))

(define-public sbcl-qlot
  (let ((commit "4d1b69729f4dc378051ed0d7edee37684eaf9db3")
        (revision "0"))
    (package
      (name "sbcl-qlot")
      (version (git-version "1.6.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/qlot")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hslxplmkfc8rjy1kxc84iax6phzndfbi5i59xcc8wqpqmlnfx68"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("qlot" "qlot/subcommands" "qlot/command")))
      (inputs
       (list sbcl-dexador
             sbcl-yason
             sbcl-lparallel
             sbcl-ironclad
             sbcl-fuzzy-match
             sbcl-deflate
             sbcl-archive
             sbcl-rove
             sbcl-assoc-utils
             sbcl-quicklisp))
      (home-page "https://qlot.tech/")
      (synopsis "A project-local library installer for Common Lisp")
      (description "")
      (license license:expat))))

sbcl-qlot
