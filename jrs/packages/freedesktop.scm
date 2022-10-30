(define-module (jrs packages freedesktop)
  #:use-module (rde packages)
  #:use-module (gnu packages gl)
  #:use-module ((gnu packages freedesktop) :prefix gnu:)
  #:use-module ((jrs packages linux) :prefix jrs:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages cran))

(define-public xdg-desktop-portal
  (package
   (inherit gnu:xdg-desktop-portal)
   (name "xdg-desktop-portal")
   (version "1.14.6")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/flatpak/xdg-desktop-portal/releases/download/"
                  version "/xdg-desktop-portal-" version ".tar.xz"))
            (sha256
             (base32
              "1q0djpnwlrqm0h0alyh1r6dlkqdrr7mj5hiam4mqzxqa5jbqkrgj"))))
   (inputs
    (modify-inputs (package-inputs gnu:xdg-desktop-portal)
                   (replace "pipewire" jrs:pipewire)))))

(define-public xdg-desktop-portal-gtk
  (package
   (inherit gnu:xdg-desktop-portal-gtk)
   (propagated-inputs
    (modify-inputs (package-propagated-inputs gnu:xdg-desktop-portal-gtk)
                   (replace "xdg-desktop-portal" xdg-desktop-portal)))))

(define-public xdg-desktop-portal-wlr
  (package
    (inherit gnu:xdg-desktop-portal-wlr)
    (name "xdg-desktop-portal-wlr")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                    (commit (string-append "v" version))))
              (file-name
               (git-file-name (package-name gnu:xdg-desktop-portal-wlr) version))
              (sha256
               (base32
                "1b02708fiz9195n71xlw8iy9dghmb5iynxppfx7wjvh8ydz68fsk"))
              (patches
               (search-patches "xdg-desktop-portal-wlr-harcoded-length.patch"))))
    (inputs
     (modify-inputs (package-inputs gnu:xdg-desktop-portal-wlr)
                    (replace "pipewire" jrs:pipewire)
                    (replace "wayland-protocols" gnu:wayland-protocols-next)
                    (prepend mesa libdrm-latest)))
    (propagated-inputs
     (list xdg-desktop-portal
           xdg-desktop-portal-gtk))))

xdg-desktop-portal
