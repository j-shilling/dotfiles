(define-module (jrs packages freedesktop)
  #:use-module ((gnu packages freedesktop) :prefix gnu:)
  #:use-module (guix packages))

(define-public xdg-desktop-portal
  (package
   (inherit gnu:xdg-desktop-portal)
   (name "xdg-desktop-portal")
   (version "1.14.16")))

(define-public xdg-desktop-portal-wlr
  (package
   (inherit gnu:xdg-desktop-portal)
   (name "xdg-desktop-portal-wlr")
   (version "1.14.16")))
