(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module (manifests doom-emacs)
  #:use-module (manifests core)
  #:use-module (manifests media)
  #:use-module (manifests browsers)
  #:use-module (manifests shell)
  #:use-module (manifests build-tools)
  #:export (default-manifest
            default-packages))

(define default-packages
  (append doom-emacs-packages
          core-packages
          shell-packages
          media-packages
          browsers-packages
          build-tools-packages))

(define default-manifest
  (packages->manifest default-packages))

default-manifest
