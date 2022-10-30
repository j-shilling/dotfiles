(define-module (jrs build npm)
  #:use-module (guix build utils)
  #:use-module (guix build download)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)

  #:export (npm-fetch))

(define* (npm-fetch name version directory
                    #:optional scope
                    #:key (npm-command "npm"))
  "Fetch an NPM package."

  (define (package-name name version scope)
    (format #f "~a~a@~a"
            (let* ((s (or scope ""))
                   (l (string-length s)))
              (if (> l 0)
                  (format #f "@~a/"
                          (substring s
                                     (if (string-match "^@" s) 1 0)
                                     (if (string-match "/$" s) (- l 1) l)))
                  s))

            name

            (if (string-match "^@" version)
                (substring version 1)
                version)))

  (define (package-url npm-command name)
    (let* ((home (mkdtemp "npm-XXXXXX"))
           (cmd (format #f "HOME=~a ~a view ~a dist.tarball" home npm-command name))
           (pipe (open-input-pipe cmd))
           (url  (read-line pipe)))
      (close-port pipe)
      url))

  (define (package-file-name url)
    (basename url))

  (mkdir-p directory)

  (with-directory-excursion directory
    (let* ((pkg-name (package-name name version scope))
           (pkg-url (package-url npm-command pkg-name))
           (pkg-file-name (package-file-name pkg-url)))
      (url-fetch pkg-url pkg-file-name
                 #:timeout 60
                 #:verify-certificate? #f
                 #:print-build-trace? #t))))
