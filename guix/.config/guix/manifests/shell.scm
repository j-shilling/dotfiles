(define-module (manifests shell)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("bash"
         "bash-completion"
         "tmux"
         "fzf"
         "direnv"
         "emacs-envrc")))

(define manifest
  (packages->manifest packages))

manifest
