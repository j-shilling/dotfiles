(define-module (manifests devel-tools)
  #:use-module (guix profiles)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages java)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages version-control)
  #:export (packages
            manifest))

(when (resolve-module '(nongnu packages clojure) #:ensure #f)
  (use-modules (nongnu packages clojure)))

(define packages
  (filter (negate unspecified?)
          (list clang
                cmake
                docker
                docker-cli
                docker-compose
                git
                gnu-make
                mysql
                (specification->package+output "openjdk:jdk")
                sbcl
                (when (defined? 'leiningen)
                  leiningen))))

(define manifest
  (packages->manifest packages))

manifest
