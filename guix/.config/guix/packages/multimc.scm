(define-module (packages multimc)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages java)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module ((nonguix licenses) :prefix non-license:)
  #:export (multimc))

(define-public multimc
  (let ((url "https://github.com/MultiMC/Launcher.git")
        (tag  "0.6.14")
        (hash "0w4b3hgz0d208vb8l92km521vj9zsfy2ga28zl6l5daxgv7kxlzf"))
    (package
      (name "multimc")
      (version tag)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url url)
                      (recursive? #t)
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 hash))))
      (build-system cmake-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (arguments
       `(#:tests? #f                    ; Tests require network access
         #:configure-flags '("-DMultiMC_LAYOUT=lin-system")))
      (inputs
       `(("jdk" ,icedtea "jdk")
         ("zlib" ,zlib)
         ("qtbase" ,qtbase-5)
         ("qtwayland" ,qtwayland)
         ("xrandr" ,xrandr)
         ("libx11" ,libx11)
         ("libxext" ,libxext)
         ("libxcursor" ,libxcursor)
         ("libxrandr" ,libxrandr)
         ("libxxf86vm" ,libxxf86vm)
         ("pulseaudio" ,pulseaudio)
         ("mesa" ,mesa)))
      (home-page "https://multimc.org/")
      (synopsis "Launcher for Minecraft")
      (description
       "This package allows you to have multiple, separate instances of
Minecraft and helps you manage them and their associated options with
a simple interface.")
      (license (list license:asl2.0      ; MultiMC
                     license:lgpl2.1     ; Qt 5
                     license:lgpl3+      ; libnbt++
                     license:gpl2+       ; rainbow (KGuiAddons), Quazip, Pack200
                     license:silofl1.1   ; Material Design Icons
                     license:expat       ; lionshead, MinGW runtime
                     license:public-domain ; xz-minidec
                     license:isc           ; Hoedown
                     license:bsd-3         ; ColumnResizer
                     ;; Batch icon set:
                     (non-license:nonfree "file://COPYING.md"))))))

multimc
