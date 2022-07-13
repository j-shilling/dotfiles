(define-module (manifests emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (list
   emacs-next-pgtk
   emacs-lispy
   emacs-guix
   emacs-geiser-guile
   emacs-use-package
   emacs-magit
   emacs-cider
   emacs-helpful
   emacs-macrostep))

(define manifest
  (packages->manifest packages))

manifest
