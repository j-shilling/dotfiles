(define-module (manifests build-tools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages llvm)
  #:use-module (guix profiles)
  #:export (build-tools-packages
            build-tools-manifest))

(define build-tools-packages
  (list texinfo
        autoconf
        automake
        libtool
        autobuild
        gnu-c-manual
        llvm-13
        clang-runtime-13
        clang-13
        clang-toolchain-13
        lld-13
        lldb))

(define build-tools-manifest
  (packages->manifest build-tools-packages))

build-tools-manifest
