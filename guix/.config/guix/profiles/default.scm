(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module (manifests doom-emacs)
  #:use-module (manifests core)
  #:use-module (manifests media)
  #:use-module (manifests browsers)
  #:export (default))

(define default
  (concatenate-manifests
   (list doom-emacs-manifest
         core-manifest
         media-manifest
         browsers-manifest)))

default