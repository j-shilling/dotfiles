(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module (manifests doom-emacs-packages)
  #:use-module (manifests media-packages)
  #:use-module (manifests general-utilities)
  #:export (default))

(define default
  (concatenate-manifests
   (list doom-emacs-manifest
         media-manifest
         general-utilities-manifest)))

default
