(define-module (manifests emacs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (list
   coreutils
   emacs-next-pgtk
   emacs-paredit
   emacs-guix
   emacs-geiser
   emacs-use-package
   emacs-magit
   emacs-cider
   emacs-helpful
   emacs-macrostep))

(define manifest
  (packages->manifest packages))

manifest
