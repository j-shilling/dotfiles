(define-module (jrs services mako)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:use-module (gnu services configuration)
  #:use-module (gnu packages wm)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (guix diagnostics)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (guix gexp)

  #:export (home-mako-extension
            home-mako-service-type
            home-mako-configuration))

(define mako-config? list?)

(define (serialize-mako-config exprs)
  (define (serialize-mako-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) (format #f "~a" e))
      ((lst ...)
       (raise (formatted-message
               (G_ "Mako term should be a non list value. Provided \
term is:\n~a") lst)))
      (e e)))

  (define (serialize-mako-expression expr)
    (match expr
      ((key . val)
       (list (serialize-mako-term key) "="
              (serialize-mako-term val) "\n"))
      (e
       (raise (formatted-message
               (G_ "Mako expression should be a list of terms, \
but provided expression is:\n ~a")
               e)))))

  (append-map serialize-mako-expression exprs))


(define-configuration home-mako-configuration
  (package
    (package mako)
    "Mako package to use.")
  (config
   (mako-config
    (list))
   "A-List of expressions. Each @dfn{expression} is a list of key-value
pairs."))

(define (add-mako-package config)
  (list (home-mako-configuration-package config)))

(define (add-mako-configuration config)
  `(("mako/config"
     ,(apply
       mixed-text-file
       "mako-config"
       (serialize-mako-config (home-mako-configuration-config config))))))

(define (mako-shepherd-service config)
  (shepherd-service
   (documentation
    "Mako service")
   (provision '(mako))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-mako-configuration-package config)
                      "/bin/mako"))
             #:log-file (string-append
                         (or (getenv "XDG_LOG_HOME")
                             (format #f "~a/.local/var/log"
                                     (getenv "HOME")))
                         "/mako.log")))
   (stop #~(make-kill-destructor))))

(define (add-mako-shepherd-service config)
  (list (mako-shepherd-service config)))

(define (home-mako-extension cfg extensions)
  (home-mako-configuration
   (inherit cfg)
   (config
    (append (home-mako-configuration-config cfg)
            (append-map identity (reverse extensions))))))

(define home-mako-service-type
  (service-type (name 'home-mako)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        add-mako-package)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        add-mako-configuration)
                       (service-extension
                        home-shepherd-service-type
                        add-mako-shepherd-service)))
                (compose identity)
                (extend home-mako-extension)
                (default-value (home-mako-configuration))
                (description "\
Install and configure mako, a notification daemon for wayland.")))
