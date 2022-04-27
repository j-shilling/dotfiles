(define-module (jake manifests)
  #:use-module (gnu packages)
  #:export (doom-emacs-packages))

(define doom-emacs-packages
  (specifications->manifest
   `("emacs-native-comp"
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
     "leiningen")))

doom-emacs-packages
