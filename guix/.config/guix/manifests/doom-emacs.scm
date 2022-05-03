(define-module (manifests doom-emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages java)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages uml)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages node)
  #:use-module (guix profiles)
  #:export (doom-emacs-packages
            doom-emacs-manifest))

(define doom-emacs-packages
  (filter (negate unspecified?)
          (list
           ripgrep
           (if (defined? 'emacs-with-native-comp)
               emacs-with-native-comp
               emacs)
           git
           fd
           binutils
           cmake
           gnu-make
           libvterm
           linux-libre-headers
           aspell
           aspell-dict-en
           sbcl
           clang
           openjdk14
           grip
           graphviz
           plantuml
           shellcheck
           tidy
           xwininfo
           xprop
           xdotool
           xclip
           mu
           isync
           node
           clojure
           (when (defined? 'leiningen)
             leiningen)
           emacs-guix
           font-fira-code)))

(define doom-emacs-manifest
  (packages->manifest doom-emacs-packages))

doom-emacs-manifest
