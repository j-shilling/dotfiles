(define-module (my packages python)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define patches
  (string-append
                 (if (current-filename)
                     (dirname (current-filename))
                     (string-append (getenv "HOME")
                                    "/dotfiles/my/packages"))
                 "/patches/"))

(define-public python-3.11
  (package
    (inherit python-3.12)
    (name "python")
    (version "3.11.9")
    (source (origin
              (inherit (package-source python-3.10))
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (sha256
               (base32
                "11svljymlyx5rmcxmjvak0g3s2inv43491kc2a8hclgw4djqj7lv"))
              (patches (cons
                        (string-append patches "python-3.11-fix-tests.patch")
                        (search-patches "python-3-deterministic-build-info.patch"
                                        "python-3-hurd-configure.patch")))))))
