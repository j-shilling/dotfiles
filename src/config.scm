(define-module (config)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features version-control)
  #:use-module (rde features xdg)
  #:use-module (rde features docker)
  #:use-module (rde features documentation)
  #:use-module (rde features python)
  #:use-module (rde features ssh)
  #:use-module (rde features markup)
  #:use-module (rde features guile)
  #:use-module (rde features terminals)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features fontutils)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages base)
  #:use-module (contrib features json)
  #:use-module (contrib features javascript)
  #:use-module (config packages node-xyz)
  #:use-module (config emacs)
  #:use-module (config shell))

;;; Code:

(define is-wsl?
  (getenv "WSL_DISTRO_NAME"))

(define %base-features
  (list
   (feature-user-info
    #:user-name "jake"
    #:full-name "Jake Shilling"
    #:email "shilling.jake@gmail.com"
    #:emacs-advanced-user? #t)
   (feature-base-packages
    #:home-packages
    (list
     devcontainers-cli-0.72.0
     (make-glibc-utf8-locales
      glibc
      #:locales (list "en_US"))))
   (feature-fonts)
   (feature-vterm)

   (feature-xdg)

   (feature-markdown)
   (feature-tex)
   (feature-python)

   (feature-compile)
   (feature-docker)

   (feature-javascript)

   (feature-guile)))

(define %emacs-features
  (emacs-features
   #:wayland? is-wsl?))

(define %shell-features
  (shell-features))

(define %all-features
  `(,@%base-features
    ,@%emacs-features
    ,@%shell-features))

(define minimal-rde-config
  (rde-config
   (features %all-features)))

(rde-config-home-environment minimal-rde-config)
