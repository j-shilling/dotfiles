(define-module (jake manifests general-utilities)
  #:use-module (gnu packages)
  #:export (general-utilities))

(define general-utilities
  (map
   (lambda (spec) (specification->package spec))
   '("zsh"
     "curl"
     "nyxt"
     "stow"
     "git"
     "pinentry"
     "gnupg"
     "openssh"
     "tree")))

general-utilities
