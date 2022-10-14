(define-module (jrs manifests core)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define specifications
  '("nss-certs"
    "curl"
    "git"
    "bash"
    "bash-completion"
    "emacs"
    "vim"
    "coreutils"
    "file"
    "make"
    "stow"))

(define packages
  (map (compose list specification->package)
       specifications))

(define manifest
  (packages->manifest packages))

manifest
