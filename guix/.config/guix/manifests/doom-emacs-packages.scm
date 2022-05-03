(define-module (manifests doom-emacs-packages)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (doom-emacs-specifications
            doom-emacs-packages
            doom-emacs-manifest))

(define doom-emacs-specifications '("ripgrep"
                                    "emacs-native-comp"
                                    "git"
                                    "fd"
                                    "binutils"
                                    "cmake"
                                    "make"
                                    "libvterm"
                                    "linux-libre-headers"
                                    "aspell"
                                    "aspell-dict-en"
                                    "sbcl"
                                    "clang"
                                    "openjdk"
                                    "grip"
                                    "graphviz"
                                    "plantuml"
                                    "shellcheck"
                                    "tidy"
                                    "xwininfo"
                                    "xprop"
                                    "xdotool"
                                    "xclip"
                                    "mu"
                                    "isync"
                                    "node"
                                    "clojure"
                                    "clojure-tools"
                                    "leiningen"
                                    "emacs-guix"
                                    "font-fira-code"))

(define doom-emacs-packages
  (map
   (lambda (spec) (specification->package spec))
   doom-emacs-specifications))

(define doom-emacs-manifest
  (packages->manifest doom-emacs-packages))

doom-emacs-manifest
