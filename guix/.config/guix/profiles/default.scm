(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module (manifests doom-emacs)
  #:use-module (manifests core)
  #:use-module (manifests media)
  #:use-module (manifests browsers)
  #:use-module (manifests shell)
  #:use-module (manifests build-tools)
  #:use-module ((manifests sway) :prefix sway:)
  #:use-module ((manifests devel-tools) :prefix devel:)
  #:use-module ((manifests games) :prefix games:)
  #:export (manifest
            packages))

(define packages
  (filter (negate unspecified?)
          (append doom-emacs-packages
                  core-packages
                  shell-packages
                  media-packages
                  browsers-packages
                  build-tools-packages
                  sway:packages
                  devel:packages
                  games:packages)))

(define manifest
  (packages->manifest packages))

manifest
