(define-module (manifests version-control)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix profiles)
  #:export (packages
	    manifest))

(define packages
  (list git
	emacs-magit))

(define manifest
  (packages->manifest packages))

manifest
