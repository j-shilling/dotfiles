(define-module (jrs packages ocaml-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy))

(define-public ocaml-manual
  (package
    (name "ocaml-manual")
    (version "4.14")
    (source (origin (method url-fetch)
                    (uri
                     (format #f "https://ocaml.org/releases/~a/ocaml-~a-refman.info.tar.gz"
                             version version))
                    (sha256
                     (base32
                      "13h3iwn24wwdhjqkf4i4fncjg1mrg764g0k6kmc1gfiwisbkrps2"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("./" "share/info/"))))
    (synopsis "")
    (description "")
    (home-page "https://v2.ocaml.org/manual/index.html")
    (license #f)))
