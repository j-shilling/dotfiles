(define-module (jrs utils)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-34)
  #:export (guix-system?))

(define (linux?)
  (equal? (vector-ref (uname) 0)
          "Linux"))

(define (os-release)
  (guard (_
          (else #f))
    (call-with-input-file "/etc/os-release"
      (lambda (port)
        (let loop ((line (read-line port))
                   (result '()))
          (if (eof-object? line)
              (reverse result)
              (begin
                (let* ((split (string-split line #\=))
                       (key (string->symbol (car split)))
                       (val (if (string-match "\".*\"" (cadr split))
                                (substring (cadr split) 1 (- (string-length (cadr split)) 1))
                                (string->symbol (cadr split))))
                       (d (cons key val)))
                  (loop (read-line port)
                        (cons d result))))))))))

(define (linux-distro-id)
  (if (linux?)
      (assq-ref (os-release) 'ID)
      #f))

(define (guix-system?)
  (eq? (linux-distro-id) 'guix))
