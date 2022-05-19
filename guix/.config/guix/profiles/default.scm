(define-module (profiles default)
  #:use-module (guix profiles)
  #:use-module (manifests doom-emacs)
  #:use-module (manifests core)
  #:use-module (manifests media)
  #:use-module (manifests browsers)
  #:use-module (manifests shell)
  #:use-module (manifests build-tools)
  #:use-module ((manifests sway) :prefix sway:)
  #:export (manifest
            packages))

(when (resolve-module '(manifests desktop) #:ensure #f)
  (use-modules (manifests desktop)))

(define packages
  (filter (negate unspecified?)
          (append doom-emacs-packages
                  core-packages
                  shell-packages
                  media-packages
                  browsers-packages
                  build-tools-packages
                  sway:packages
                  (when (defined? 'desktop-packages)
                    desktop-packages))))

(define manifest
  (packages->manifest packages))

manifest
