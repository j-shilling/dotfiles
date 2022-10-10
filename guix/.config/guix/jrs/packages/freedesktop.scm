(define-module (jrs packages freedesktop)
  #:use-module ((gnu packages freedesktop) :prefix gnu:)
  #:use-module ((jrs packages linux) :prefix jrs:)
  #:use-module (guix packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux))

(define pipewire jrs:pipewire)

(define-public xdg-desktop-portal
  (package
   (inherit gnu:xdg-desktop-portal)
   (name "xdg-desktop-portal")
   (version "1.14.16")
   (inputs
    (modify-inputs (package-inputs gnu:xdg-desktop-portal)
                   (replace "pipewire" pipewire)))))

(define-public xdg-desktop-portal-wlr
  (package
   (inherit gnu:xdg-desktop-portal)
   (name "xdg-desktop-portal-wlr")
   (version "1.14.16")
   (inputs
    (modify-inputs (package-inputs gnu:xdg-desktop-portal-wlr)
                   (replace "pipewire" pipewire)
                   (prepend glib json-glib gnu:libportal geoclue
                            fuse-3 gdk-pixbuf dbus)))))

xdg-desktop-portal
