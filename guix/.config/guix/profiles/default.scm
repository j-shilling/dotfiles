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

(when (resolve-module '(manifests desktop) #:ensure #f)
  (use-modules (manifests desktop)))

(define default-packages
  (append doom-emacs-packages
          core-packages
          shell-packages
          media-packages
          browsers-packages
          build-tools-packages
          (if (defined? 'desktop-packages)
              desktop-packages
              (list))))

(define default-manifest
  (packages->manifest default-packages))

default-manifest
