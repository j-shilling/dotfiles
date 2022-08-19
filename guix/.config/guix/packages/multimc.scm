;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

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

(define multimc
  (package
    (name "multimc")
    (version "0.6.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MultiMC/Launcher.git")
                    (recursive? #t)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kc28wx0rn8qgqdpfks0hwbq7mckad6p1ikqlgi7zigixg0zaa1y"))))
    (build-system cmake-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:tests? #f                      ; Tests require network access
       #:configure-flags '("-DLauncher_LAYOUT=lin-nodeps"
                           "-DLauncher_APP_BINARY_NAME=multimc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out            (assoc-ref outputs "out"))
                    (bin            (string-append out "/bin"))
                    (exe            (string-append bin "/multimc"))
                    (qtwayland      (assoc-ref inputs "qtwayland"))
                    (xrandr         (assoc-ref inputs "xrandr"))
                    (jdk            (assoc-ref inputs "jdk")))
               (wrap-program exe
                 `("PATH" ":" prefix (,(string-append xrandr "/bin")
                                      ,(string-append jdk "/bin")))
                 `("QT_PLUGIN_PATH" ":" prefix (,(string-append
                                                  qtwayland "/lib/qt5/plugins")))
                 `("GAME_LIBRARY_PATH" ":" prefix
                   (,@(map (lambda (dep)
                             (string-append (assoc-ref inputs dep)
                                            "/lib"))
                           '("libx11" "libxext" "libxcursor"
                             "libxrandr" "libxxf86vm" "pulseaudio" "mesa")))))
               #t)))
         ;; (add-after 'patch-paths 'install-desktop-entry
         ;;   (lambda* (#:key outputs #:allow-other-keys)
         ;;     (let* ((out (assoc-ref outputs "out"))
         ;;            (applications (string-append out "/share/applications"))
         ;;            (app-icons (string-append out "/share/icons/hicolor/scalable/apps")))
         ;;       (with-directory-excursion "../source"
         ;;         (install-file "application/package/linux/multimc.desktop"
         ;;                       applications)
         ;;         (install-file "application/resources/multimc/scalable/multimc.svg"
         ;;                       app-icons))
         ;;       #t)))
         )))
    (inputs
     `(("jdk" ,openjdk17 "jdk")
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
    (license (list license:asl2.0        ; MultiMC
                   license:lgpl2.1       ; Qt 5
                   license:lgpl3+        ; libnbt++
                   license:gpl2+         ; rainbow (KGuiAddons), Quazip, Pack200
                   license:silofl1.1     ; Material Design Icons
                   license:expat         ; lionshead, MinGW runtime
                   license:public-domain ; xz-minidec
                   license:isc           ; Hoedown
                   license:bsd-3         ; ColumnResizer
                   ;; Batch icon set:
                   (non-license:nonfree "file://COPYING.md")))))

multimc
