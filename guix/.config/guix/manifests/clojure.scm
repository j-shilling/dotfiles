(define-module (manifests clojure)
  #:use-module (gnu packages)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix profiles)
  #:export (packages
	    manifest))

(when (resolve-module '(nongnu packages clojure) #:ensure #f)
  (use-modules (nongnu packages clojure)))

(define packages
  (filter (negate unspecified?)
	  (list clojure
		clojure-tools
		emacs-cider
		emacs-clojure-mode
		emacs-clojure-snippets
		emacs-eglot
		(specification->package+output "openjdk:jdk")
		(when (defined? 'leiningen)
		  leiningen))))

(define manifest
  (packages->manifest packages))

manifest

