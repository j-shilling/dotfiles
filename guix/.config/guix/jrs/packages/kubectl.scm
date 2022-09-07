(define-module (jrs packages kubectl)
  #:use-module (srfi srfi-28)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) :prefix license:))

(define-public kubectl
  (package
   (name "kubectl")
   (version "1.25.0")
   (source (origin (method url-fetch)
                   (uri (format
                         "https://dl.k8s.io/release/v~a/bin/linux/amd64/kubectl"
                         version))
                   (sha256
                    (base32
                     "119q430fvdw6d13wljakbfn3cjhrk6agndpfv0i5rj8q484wfg72"))))
   (home-page "https://kubernetes.io")
   (synopsis "Kubectl")
   (description "")
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("./kubectl" "bin/"))
      #:phases
      (modify-phases %standard-phases
                     (add-before 'install 'set-permissions
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (chmod "./kubectl" #o755))))))
   (license #:f)))

kubectl
