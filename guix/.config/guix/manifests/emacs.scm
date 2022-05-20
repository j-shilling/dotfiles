(define-module (manifests emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix profiles))

(define packages
  (list
   emacs
   emacs-lispy
   emacs-guix
   emacs-geiser-guile
   emacs-use-package))

(define manifest
  (packages->manifest packages))

manifest
