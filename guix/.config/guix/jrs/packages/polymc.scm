(define-module (jrs packages polymc)
  #:use-module (guix profiles)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages java)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module ((nonguix licenses) :prefix non-license:))

(define-public polymc
  (package
   (name "polymc")
   (version "1.4.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/PolyMC/PolyMC.git")
                  (recursive? #t)
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0y6nazjxg6yxp2ifpbn1ank51g0zcwmm139qagjxpbhjjzny98ls"))))
   (build-system cmake-build-system)
   (supported-systems '("i686-linux" "x86_64-linux"))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-after 'install 'patch-paths
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (let* ((out            (assoc-ref outputs "out"))
                                         (bin            (string-append out "/bin/polymc"))
                                         (xrandr         (assoc-ref inputs "xrandr"))
                                         (qtwayland      (assoc-ref inputs "qtwayland")))
                                    (wrap-program bin
                                                  `("PATH" ":" prefix (,(string-append xrandr "/bin")))
                                                  `("QT_PLUGIN_PATH" ":" prefix (,(string-append
                                                                                   qtwayland "/lib/qt5/plugins")))
                                                  `("LD_LIBRARY_PATH" ":" prefix
                                                    (,@(map (lambda (dep)
                                                              (string-append (assoc-ref inputs dep)
                                                                             "/lib"))
                                                            '("libx11" "libxext" "libxcursor"
                                                              "libxrandr" "libxxf86vm" "pulseaudio" "mesa")))))
                                    #t))))))
   (native-inputs (list extra-cmake-modules))
   (inputs (list zlib
                 qtbase-5
                 qtwayland
                 xrandr
                 libx11
                 libxext
                 libxcursor
                 libxrandr
                 libxxf86vm
                 pulseaudio
                 mesa))
   (propagated-inputs (list `(,openjdk18 "jdk")))
   (home-page "https://polymc.org/")
   (synopsis "A free, open source launcher for Minecraft")
   (description
    "Allows you to have multiple, separate instances of Minecraft (each with
their own mods, texture packs, saves, etc) and helps you manage them and
their associated options with a simple interface.")
   (license (list license:gpl3     ; PolyMC, launcher
                  license:expat    ; MinGW runtime, lionshead, tomlc99
                  license:lgpl3    ; Qt 5/6
                  license:lgpl3+   ; libnbt++
                  license:lgpl2.1+ ; rainbow (KGuiAddons)
                  license:isc      ; Hoedown
                  license:silofl1.1     ; Material Design Icons
                  license:lgpl2.1       ; Quazip
                  license:public-domain ; xz-minidec, murmur2, xz-embedded
                  license:bsd-3  ; ColumnResizer, O2 (Katabasis fork),
                                        ; gamemode, localpeer
                  license:asl2.0        ; classparser, systeminfo
                  ;; Batch icon set:
                  (non-license:nonfree "file://COPYING.md")))))

polymc
