(define-module (manifests doom-emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages uml)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix profiles)
  #:export (doom-emacs-packages
            doom-emacs-manifest))

(when (resolve-module '(flat packages emacs) #:ensure #f)
  (use-modules (flat packages emacs)))

(when (resolve-module '(nongnu packages clojure) #:ensure #f)
  (use-modules (nongnu packages clojure)))

(define doom-emacs-packages
  (filter (negate unspecified?)
          (list aspell
                aspell-dict-en
                binutils
                clang
                clojure
                cmake
                emacs-guix
                fd
                font-fira-code
                font-fira-sans
                git
                gnu-make
                graphviz
                grip
                isync
                libvterm
                linux-libre-headers
                mu
                node
                openjdk14
                plantuml
                ripgrep
                sbcl
                shellcheck
                tidy
                xclip
                xdotool
                xprop
                xwininfo
                emacs
                (when (defined? 'leiningen)
                  leiningen))))

(define doom-emacs-manifest
  (packages->manifest doom-emacs-packages))

doom-emacs-manifest
