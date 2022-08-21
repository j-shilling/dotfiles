(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module (gnu packages)
  #:use-module (manifests media)
  #:use-module ((manifests browsers) :prefix browsers:)
  #:use-module ((manifests shell) :prefix shell:)
  #:use-module ((manifests emacs) :prefix emacs:)
  #:use-module ((manifests sway) :prefix sway:)
  #:use-module ((manifests games) :prefix games:)
  #:use-module ((profiles programming) :prefix programming:)
  #:export (manifest
            packages))

(define packages
  (filter (negate unspecified?)
          (append shell:packages
                  media-packages
                  browsers:packages
                  programming:packages
                  sway:packages
                  games:packages
                  (list (specification->package+output "flatpak")))))

(define manifest
  (packages->manifest packages))

manifest
