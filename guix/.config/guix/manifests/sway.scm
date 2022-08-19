(define-module (manifests sway)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages man)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages pkg-config)
  #:export (manifest
            packages))

(define-public fuzzel
  (package
   (name "fuzzel")
   (version "1.7.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://codeberg.org/dnkl/fuzzel.git")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1261gwxiky37pvzmmbrpml1psa22kkglb141ybj1fbnwg6j7jvlf"))))
   (build-system meson-build-system)
   (inputs
    `((,wayland-protocols "out")
      (,scdoc "out")
      (,pkg-config "out")
      (,tllist "out")
      (,pixman "out")
      (,wayland "out")
      (,libxkbcommon "out")
      (,cairo "out")
      (,libpng "out")
      (,librsvg "out")
      (,fcft "out")
      (,fontconfig "out")))
   (home-page "https://codeberg.org/dnkl/fuzzel")
   (synopsis "Wayland-native application launcher")
   (description
    "Fuzzel is a Wayland-native application launcher, similar to rofi's
drun mode.")
   (license (list license:expat
                  license:zlib))))

(define packages
  (list alacritty
        bemenu
        brightnessctl
        font-fira-code
        font-fira-mono
        font-fira-sans
        mako
        pavucontrol
        sway
        waybar
        fuzzel))

(define manifest
  (packages->manifest packages))

manifest
