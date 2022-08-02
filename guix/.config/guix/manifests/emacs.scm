(define-module (manifests emacs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages guile)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (list
   coreutils
   bash
   gzip
   guile-3.0-latest
   glibc-locales
   emacs-next-pgtk
   emacs-paredit
   emacs-guix
   emacs-geiser))

(define manifest
  (packages->manifest packages))

manifest
