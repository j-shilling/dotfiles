(define-module (jrs manifests core)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("nss-certs"
         "curl"
         "git"
         "bash"
         "bash-completion"
         "emacs"
         "vim"
         "coreutils"
         "file")))

(define manifest
  (packages->manifest packages))

manifest
