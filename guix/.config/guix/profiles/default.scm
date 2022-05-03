(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module (manifests doom-emacs)
  #:use-module (manifests media-packages)
  #:use-module (manifests core)
  #:export (default))

(define default
  (concatenate-manifests
   (list doom-emacs-manifest
         media-manifest
         core-manifest)))

default
