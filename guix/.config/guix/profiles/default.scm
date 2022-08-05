(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module ((manifests core) :prefix core:)
  #:use-module (manifests media)
  #:use-module (manifests browsers)
  #:use-module (manifests shell)
  #:use-module (manifests build-tools)
  #:use-module ((manifests emacs) :prefix emacs:)
  #:use-module ((manifests sway) :prefix sway:)
  #:use-module ((manifests devel-tools) :prefix devel:)
  #:use-module ((manifests games) :prefix games:)
  #:use-module ((profiles programming) :prefix programming:)
  #:export (manifest
            packages))

(define packages
  (filter (negate unspecified?)
          (append core:packages
                  shell-packages
                  media-packages
                  browsers-packages
		  programming:packages
                  sway:packages
                  games:packages)))

(define manifest
  (packages->manifest packages))

manifest
