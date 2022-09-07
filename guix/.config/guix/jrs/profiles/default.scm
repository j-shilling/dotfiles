(define-module (jrs profiles default)
  #:use-module (guix profiles)
  #:use-module (gnu packages)
  #:use-module ((jrs manifests browsers) :prefix browsers:)
  #:use-module ((jrs manifests clojure) :prefix clojure:)
  #:use-module ((jrs manifests emacs) :prefix emacs:)
  #:use-module ((jrs manifests games) :prefix games:)
  #:use-module ((jrs manifests lisp) :prefix lisp:)
  #:use-module ((jrs manifests media) :prefix media:)
  #:use-module ((jrs manifests shell) :prefix shell:)
  #:use-module ((jrs manifests sway) :prefix sway:)
  #:use-module ((jrs manifests version-control) :prefix version-control:)
  #:use-module ((jrs manifests terraform) :prefix terraform:)
  #:use-module ((jrs manifests core) :prefix core:)
  #:use-module ((jrs manifests file-formats) :prefix file-formats:)
  #:export (manifest
            packages))

(define packages
  (filter (negate unspecified?)
          (append browsers:packages
                  core:packages
                  clojure:packages
                  emacs:packages
                  games:packages
                  lisp:packages
                  media:packages
                  shell:packages
                  sway:packages
                  version-control:packages
                  terraform:packages
                  file-formats:packages
                  (map (compose list specification->package+output)
                       '("flatpak")))))

(define manifest
  (packages->manifest packages))

manifest
