(define-module (jrs packages dpkg)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses)
  #:use-module ((guix licenses) :prefix license:))

(define-public start-stop-daemon
  (package
    (name "start-stop-daemon")
    (version "1.20.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.dpkg.org/git/dpkg/dpkg.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dj9jbx40ipdmzd58hmwgmmgjgvywy1ja2zh40r39y2mxddrw2sd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:bootstrap-scripts (list "autogen")
       #:make-flags (list "-C" "utils")
       #:configure-flags (list "--enable-start-stop-daemon"
                               "--disable-update-alternatives")
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-autogen-shebang
           (lambda* (#:key bootstrap-scripts #:allow-other-keys)
             (for-each patch-shebang (append bootstrap-scripts))
             (let ((get-version "printf \"%s\" \"1.20.12\"")
                   (port (open-file "get-version" "w")))
               (format port get-version)
               (close-port port))))
         (add-after 'configure 'build-lib
           (lambda* (#:rest _)
             (invoke "make" "-C" "lib"))))))
    (home-page "https://git.dpkg.org/cgit/dpkg/dpkg.git/")
    (synopsis "start-stop-daemon")
    (description "start-stop-daemon")
    (inputs
     (list autoconf
           gnu-gettext
           automake
           libtool
           pkg-config
           perl
           ncurses))
    (license license:gpl2)))
