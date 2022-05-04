(define-module (manifests core)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages wget)
  #:use-module (guix profiles)
  #:export (core-specifications
            core-packages
            core-manifest))

(define core-packages
  (list bash
        curl
        emacs
        git
        glibc-utf8-locales
        gnupg
        nss-certs
        openssh
        pinentry
        stow
        vim
        wget
        which
        zsh
        coreutils))

(define core-manifest
  (packages->manifest core-packages))

core-manifest
