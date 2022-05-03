(define-module (manifests general-utilities)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (general-utilities-specifications
            general-utilities-packages
            general-utilities-manifest))

(define general-utilities-specifications '("zsh"
                                           "curl"
                                           "nyxt"
                                           "stow"
                                           "git"
                                           "pinentry"
                                           "gnupg"
                                           "openssh"
                                           "tree"))

(define general-utilities-packages
  (map
   (lambda (spec) (specification->package spec))
   general-utilities-specifications))

(define general-utilities-manifest
  (packages->manifest general-utilities-packages))

general-utilities-manifest
