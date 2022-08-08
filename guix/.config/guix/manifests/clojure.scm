(define-module (manifests clojure)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("clojure"
         "clojure-tools"
         "openjdk:jdk"
         "emacs-cider"
         "emacs-clojure-mode"
         "emacs-clojure-snippets"
         "emacs-eglot")))

(define manifest
  (packages->manifest packages))

manifest
