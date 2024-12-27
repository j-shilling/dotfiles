(define-module (config)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages certs)
  #:use-module (gnu home services)
  #:use-module (gnu home services syncthing)
  #:use-module (config features guix)
  #:use-module (contrib features json)
  ;; #:use-module (contrib features javascript)
  #:use-module (config packages node-xyz)
  #:use-module (config emacs)
  #:use-module (config tools shell)
  #:use-module (config tools syncthing)
  #:use-module (config tools aws)
  #:use-module (config tools mail)
  #:use-module (config development python)
  #:use-module (config development guile)
  #:use-module (config development common-lisp)
  #:use-module (config development haskell)
  #:use-module (config development terraform))

;;; Code:

(define is-wsl?
  (getenv "WSL_DISTRO_NAME"))

(define locale
  (make-glibc-utf8-locales glibc #:locales (list "en_US")))

(define %base-features
  (list
   (feature-user-info
    #:user-name "jake"
    #:full-name "Jake Shilling"
    #:email "shilling.jake@gmail.com"
    #:emacs-advanced-user? #t)
   (feature-guix #:profile (getenv "GUIX_PROFILE"))
   (feature-custom-services
    #:home-services
    (list
     (simple-service 'install-glibc-locales
                     home-profile-service-type
                     (list locale))
     (simple-service 'set-locale-path
                     home-environment-variables-service-type
                     `(("GUIX_LOCPATH" . ,(file-append locale "/lib/locale"))))
     (simple-service 'install-nss-certs
                     home-profile-service-type
                     (list nss-certs))
     (simple-service 'set-nss-certs-path
                     home-environment-variables-service-type
                     `(("SSL_CERT_DIR" . ,(file-append nss-certs "/etc/ssl/certs"))
                       ("SSL_CERT_FILE" . "${HOME}/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")))))
   (feature-fonts)
   (feature-vterm)
   (feature-xdg)
   (feature-markdown)
   (feature-tex)
   (feature-compile)
   (feature-docker)
   ;; (feature-javascript)
   ))

(define %emacs-features
  (emacs-features
   #:wayland? is-wsl?
   #:wsl? is-wsl?))

(define %tools-features
  `(,@(shell-features)
    ,@(syncthing-features)
    ,@(aws-features)
    ,@(mail-features)))

(define %development-features
  `(,@(python-features)
    ,@(guile-features)
    ,@(common-lisp-features)
    ,@(haskell-features)
    ,@(terraform-features)))

(define %all-features
  `(,@%base-features
    ,@%emacs-features
    ,@%tools-features
    ,@%development-features))

(define minimal-rde-config
  (rde-config
   (features %all-features)))

(rde-config-home-environment minimal-rde-config)
