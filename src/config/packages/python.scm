(define-module (config packages python)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages python))

(define-public python-3.11
  (package
    (inherit python-3.10)
    (name "python3.11")
    (version "3.11.9")
    (source (origin
              (inherit (package-source python-3.10))
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (sha256
               (base32
                "11svljymlyx5rmcxmjvak0g3s2inv43491kc2a8hclgw4djqj7lv"))
              (patches '())))))

python-3.11
