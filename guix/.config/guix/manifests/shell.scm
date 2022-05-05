(define-module (manifests shell)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages terminals)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:export (shell-packages
            shell-manifest))

(define shell-packages
  (list
   bash
   bash-completion
   tmux
   fzf))

(define shell-manifest
  (packages->manifest shell-packages))

shell-manifest
