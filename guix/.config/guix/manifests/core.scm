(define-module (manifests core)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages wget)
  #:use-module (guix profiles)
  #:export (packages
	    manifest))

(define packages
  (list bash
        coreutils
        curl
        emacs
        git
        glibc-locales
        gnupg
	guile
        nss-certs
        openssh
        password-store
        pinentry
        stow
        vim
        wget
        which
        zsh))

(define manifest
  (packages->manifest core-packages))

manifest
