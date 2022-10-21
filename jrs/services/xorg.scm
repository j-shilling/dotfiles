(define-module (jrs services xorg)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages wm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (rde home services wm)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-i3-configuration
            home-i3-service-type))

;;;
;;; i3.
;;;

(define i3-config? list?)
(define (serialize-i3-config val)
  (define (aligner nestness)
    (apply string-append
           (map (const "    ") (iota nestness))))

  (define (serialize-i3-term term)
    ;; (format #t "finval. ~a\n" term)
    (match term
      (#t "yes")
      (#f "no")
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ;; TODO: Change it to ((? string? e) (format #f "~s" e))
      ((? string? e) e)
      ((lst ...)
       (raise (formatted-message
               (G_ "I3 term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
      (e e)))

  (define* (serialize-i3-expression
            expr #:optional (nestness 0))
    ;; (format #t "expres. ~a\n" expr)
    (match expr
      ;; subconfig has the same structure as config,
      ;; the only difference: it's not a top-level form
      ;; can be found at the end of expression.
      ;; (term subconfig)
      ((term ((expressions ...) ...))
       ;; (format #t "subtop. ~a . ~a\n" term expressions)
       (append
        (list (serialize-i3-term term) " {\n")
        (serialize-i3-subconfig expressions (1+ nestness))
        `(,(aligner nestness)
          "}\n")))

      ;; subexpression:
      ;; (term . rest)
      ((term rest ..1)
       ;; (format #t "inside. ~a . ~a\n" term rest)
       (cons* (serialize-i3-term term) " "
              (serialize-i3-expression rest)))

      ;; last element of subexpression
      ((term)
       ;; (format #t "term.   ~a\n" term)
       (list (serialize-i3-term term) "\n"))

      (e
       (raise (formatted-message
               (G_ "I3 expression should be a list of terms \
optionally ending with subconfigs, but provided expression is:\n ~a")
               e)))))

  (define* (serialize-i3-subconfig
            subconfig #:optional (nestness 0))
    (match subconfig
      ;; config:
      ;; ((expr1) (expr2) (expr3))
      (((expressions ...) ...)
       (append-map
        (lambda (e)
          (append (list (aligner nestness))
                  (serialize-i3-expression e nestness)))
        expressions))
      (e
       (raise (formatted-message
               (G_ "I3 (sub)config should be a list of expressions, \
where each expression is also a list, but provided value is:\n ~a") e))) ))

  (serialize-i3-subconfig val))

(define-configuration home-i3-configuration
  (package
    (package i3-gaps)
    "i3 package to use.")
  (config
   (i3-config
    `((include ,(file-append i3-gaps "/etc/i3/config"))))
   "List of expressions."))

(define (add-i3-packages config)
  (list (home-i3-configuration-package config)))

(define (add-i3-configuration config)
  `(("i3/config"
     ,(apply
       mixed-text-file
       "i3-config"
       (serialize-i3-config (home-i3-configuration-config config))))))

(define (home-i3-extensions cfg extensions)
  (home-i3-configuration
   (inherit cfg)
   (config
    (append (home-i3-configuration-config cfg)
            (append-map identity (reverse extensions))))))

(define home-i3-service-type
  (service-type (name 'home-i3)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        add-i3-packages)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        add-i3-configuration)))
                (compose identity)
                (extend home-i3-extensions)
                (default-value (home-i3-configuration))
                (description "Install and configure i3")))
