(define-module (jake manifests doom-emacs-packages)
  #:use-module (gnu packages)
  #:export (doom-emacs-packages))

(define doom-emacs-packages
  (map
   (lambda (spec) (specification->package spec))
   '("emacs-native-comp"
     "ripgrep"
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
     "emacs-guix")))

doom-emacs-packages
