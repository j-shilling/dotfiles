(define-module (jrs packages node)
  #:use-module (srfi srfi-28)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (jrs packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module ((guix licenses) :prefix license:))

;https://nodejs.org/dist/v16.17.1/node-v16.17.1-linux-x64.tar.xz
(define-public node-bin
  (package
   (name "node")
   (version "16.17.1")
   (source (origin (method url-fetch)
                   (uri
                    (format
                     "https://nodejs.org/dist/v~a/node-v~a-linux-x64.tar.xz"
                     version version))
                   (sha256
                    (base32
                     "1143v9zjw2pq6cmqksncdhpj30rzfm27mj2qbxzrd1d39arjxfh6"))))
   (home-page "https://nodejs.org")
   (synopsis "Evented I/O for V8 JavaScript")
   (description
    "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
   (build-system copy-build-system)
   (arguments
    '(#:validate-runpath? #f
      #:install-plan
      '(("./bin/" "bin/")
        ("./include/" "include/")
        ("./lib/" "lib/")
        ("./share/" "share/"))))
   (inputs
    `((,gcc "lib")))
   (license license:expat)))

; https://nodejs.org/dist/v16.17.1/node-v16.17.1.tar.gz
(define-public node
  (package
    (name "node")
    (version "16.17.1")
    (source (origin (method url-fetch)
                    (uri
                     (format
                      "https://nodejs.org/dist/v~a/node-v~a.tar.gz"
                      version version))
                    (sha256
                     (base32
                      "03x9nxl0365ajk7kky6dav6kxs3am5bdn6hrk9ph5chrc1grh8z4"))))
    (home-page "https://nodejs.org")
    (synopsis "Evented I/O for V8 JavaScript")
    (description
     "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (build-system gnu-build-system)
    (arguments
     `(
       ;; #:configure-flags '("--shared-cares"
       ;;                     "--shared-http-parser"
       ;;                     "--shared-libuv"
       ;;                     "--shared-nghttp2"
       ;;                     "--shared-openssl"
       ;;                     "--shared-zlib"
       ;;                     "--without-snapshot"
       ;;                     "--with-intl=system-icu")
       ;; Run only the CI tests.  The default test target requires additional
       ;; add-ons from NPM that are not distributed with the source.
       ;; #:test-target "test-ci-js"
       #:tests? #f
       #:modules
       ((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1)
        (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-hardcoded-program-references
           (lambda* (#:key inputs #:allow-other-keys)

             ;; Fix hardcoded /bin/sh references.
             (substitute*
                 (let ((common
                        '("lib/child_process.js"
                          "lib/internal/v8_prof_polyfill.js"
                          "test/parallel/test-child-process-spawnsync-shell.js"
                          "test/parallel/test-stdio-closed.js"
                          "test/sequential/test-child-process-emfile.js"))
                       ;; not in bootstap node:
                       (sigxfsz "test/parallel/test-fs-write-sigxfsz.js"))
                   (if (file-exists? sigxfsz)
                       (cons sigxfsz common)
                       common))
               (("'/bin/sh'")
                (string-append "'"  (search-input-file inputs "/bin/sh") "'")))

             ;; Fix hardcoded /usr/bin/env references.
             (substitute* '("test/parallel/test-child-process-default-options.js"
                            "test/parallel/test-child-process-env.js"
                            "test/parallel/test-child-process-exec-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (assoc-ref inputs "coreutils")
                               "/bin/env'")))))
         (add-after 'patch-hardcoded-program-references
             'delete-problematic-tests
           (lambda* (#:key inputs #:allow-other-keys)

             ;; This requires a DNS resolver.
             (delete-file "test/parallel/test-dns.js")

             ;; This test is timing-sensitive, and fails sporadically on
             ;; slow, busy, or even very fast machines.
             (delete-file "test/parallel/test-fs-utimes.js")

             ;; FIXME: This test fails randomly:
             ;; https://github.com/nodejs/node/issues/31213
             (delete-file "test/parallel/test-net-listen-after-destroying-stdin.js")

             ;; FIXME: These tests fail on armhf-linux:
             ;; https://github.com/nodejs/node/issues/31970
             ,@(if (string-prefix? "arm" (%current-system))
                   '((for-each delete-file
                               '("test/parallel/test-zlib.js"
                                 "test/parallel/test-zlib-brotli.js"
                                 "test/parallel/test-zlib-brotli-flush.js"
                                 "test/parallel/test-zlib-brotli-from-brotli.js"
                                 "test/parallel/test-zlib-brotli-from-string.js"
                                 "test/parallel/test-zlib-convenience-methods.js"
                                 "test/parallel/test-zlib-random-byte-pipes.js"
                                 "test/parallel/test-zlib-write-after-flush.js")))
                   '())

             ;; These tests have an expiry date: they depend on the validity of
             ;; TLS certificates that are bundled with the source.  We want this
             ;; package to be reproducible forever, so remove those.
             ;; TODO: Regenerate certs instead.
             (for-each delete-file
                       '("test/parallel/test-tls-passphrase.js"
                         "test/parallel/test-tls-server-verify.js"))))
         ;; (add-before 'configure 'set-bootstrap-host-rpath
         ;;   (lambda* (#:key native-inputs inputs #:allow-other-keys)
         ;;     (let* ((inputs      (or native-inputs inputs))
         ;;            (c-ares      (assoc-ref inputs "c-ares"))
         ;;            (http-parser (assoc-ref inputs "http-parser"))
         ;;            (icu4c       (assoc-ref inputs "icu4c"))
         ;;            (nghttp2     (assoc-ref inputs "nghttp2"))
         ;;            (openssl     (assoc-ref inputs "openssl"))
         ;;            (libuv       (assoc-ref inputs "libuv"))
         ;;            (zlib        (assoc-ref inputs "zlib")))
         ;;       (substitute* "deps/v8/gypfiles/v8.gyp"
         ;;         (("'target_name': 'torque'," target)
         ;;          (string-append target
         ;;                         "'ldflags': ['-Wl,-rpath="
         ;;                         c-ares "/lib:"
         ;;                         http-parser "/lib:"
         ;;                         icu4c "/lib:"
         ;;                         nghttp2 "/lib:"
         ;;                         openssl "/lib:"
         ;;                         libuv "/lib:"
         ;;                         zlib "/lib"
         ;;                         "'],"))))))
         (replace 'configure
           ;; Node's configure script is actually a python script, so we can't
           ;; run it with bash.
           (lambda* (#:key outputs (configure-flags '()) native-inputs inputs
                     #:allow-other-keys)
             (let* ((prefix (assoc-ref outputs "out"))
                    (xflags ,(if (%current-target-system)
                                 `'("--cross-compiling"
                                    ,(string-append
                                      "--dest-cpu="
                                      (match (%current-target-system)
                                        ((? (cut string-prefix? "arm" <>))
                                         "arm")
                                        ((? (cut string-prefix? "aarch64" <>))
                                         "arm64")
                                        ((? (cut string-prefix? "i686" <>))
                                         "ia32")
                                        ((? (cut string-prefix? "x86_64" <>))
                                         "x64")
                                        ((? (cut string-prefix? "powerpc64" <>))
                                         "ppc64")
                                        (_ "unsupported"))))
                                 ''()))
                    (flags (cons (string-append "--prefix=" prefix)
                                 (append xflags configure-flags))))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               ;; Node's configure script expects the CC environment variable to
               ;; be set.
               (setenv "CC_host" "gcc")
               (setenv "CXX_host" "g++")
               (setenv "CC" ,(cc-for-target))
               (setenv "CXX" ,(cxx-for-target))
               (setenv "PKG_CONFIG" ,(pkg-config-for-target))
               (apply invoke
                      (let ((inpts (or native-inputs inputs)))
                        (with-exception-handler
                            (lambda (e)
                              (if (search-error? e)
                                  (search-input-file inpts "/bin/python3")
                                  (raise-exception e)))
                          (lambda ()
                            (search-input-file inpts "/bin/python"))
                          #:unwind? #t))
                      "configure"
                      flags))))
         (add-after 'patch-shebangs 'patch-nested-shebangs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Based on the implementation of patch-shebangs
             ;; from (guix build gnu-build-system).
             (let ((path (append-map (match-lambda
                                       ((_ . dir)
                                        (list (string-append dir "/bin")
                                              (string-append dir "/sbin")
                                              (string-append dir "/libexec"))))
                                     (append outputs inputs))))
               (for-each
                (lambda (file)
                  (patch-shebang file path))
                (find-files (search-input-directory outputs "lib/node_modules")
                            (lambda (file stat)
                              (executable-file? file))
                            #:stat lstat)))))
         (add-after 'install 'install-npmrc
           ;; Note: programs like node-gyp only receive these values if
           ;; they are started via `npm` or `npx`.
           ;; See: https://github.com/nodejs/node-gyp#npm-configuration
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (with-output-to-file
                   ;; Use the config file "primarily for distribution
                   ;; maintainers" rather than "{prefix}/etc/npmrc",
                   ;; especially because node-build-system uses --prefix
                   ;; to install things to their store paths:
                   (string-append out "/lib/node_modules/npm/npmrc")
                 (lambda ()
                   ;; Tell npm (mostly node-gyp) where to find our
                   ;; installed headers so it doesn't try to
                   ;; download them from the internet:
                   (format #t "nodedir=~a\n" out)))))))))
    (native-inputs
     ;; Runtime dependencies for binaries used as a bootstrap.
     (list c-ares
           http-parser
           icu4c
           libuv
           `(,nghttp2 "lib")
           openssl-1.1
           zlib
           ;; Regular build-time dependencies.
           perl
           pkg-config
           procps
           python
           util-linux))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (inputs
     (list bash-minimal
           coreutils
           c-ares
           http-parser
           icu4c
           libuv
           `(,nghttp2 "lib")
           openssl
           python-wrapper               ;for node-gyp (supports python3)
           zlib))
    (license license:expat)))

node
