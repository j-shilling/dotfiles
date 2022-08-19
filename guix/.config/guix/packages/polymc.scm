(define-module (packages polymc)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages java)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module ((nonguix licenses) :prefix non-license:)
  #:export (polymc))



polymc
