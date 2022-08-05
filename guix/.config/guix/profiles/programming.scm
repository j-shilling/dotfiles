(define-module (profiles programming)
  #:use-module ((manifests emacs) :prefix emacs:)
  #:use-module ((manifests clojure) :prefix clojure:)
  #:use-module ((manifests version-control) :prefix vc:)
  #:use-module (guix profiles)
  #:export (manifest
	    packages))

(define packages
  (append emacs:packages
	  clojure:packages
	  vc:packages))

(define manifest
  (packages->manifest packages))

manifest
