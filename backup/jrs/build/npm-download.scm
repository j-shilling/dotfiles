(define-module (jrs build npm-download)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix store)

  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)

  #:export (npm-reference
            npm-fetch))

(define-record-type* <npm-reference>
  npm-reference make-npm-reference
  npm-reference?
  (name     npm-reference-name)
  (version  npm-reference-version)
  (scope    npm-reference-scope
            (default #f))
  (node     npm-reference-node
            (default #f)))

(define (node-package)
  "Return the default NPM package."
  (let ((distro (resolve-interface '(gnu packages node))))
    (module-ref distro 'node-lts)))

(define* (npm-fetch ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile))
                    (node (node-package)))

  (define modules
    (source-module-closure '((jrs build npm))
                           #:select?
                           (lambda (name)
                             (match name
                               (('jrs _ ...) #t)
                               (('guix _ ...) #t)
                               (_ #f)))))

  (define resolved-node
    (if (npm-reference-node ref)
        (npm-reference-node ref)
        node))

  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'gnutls))

  (define build
    (with-imported-modules modules
      (with-extensions (list guile-json
                             gnutls)
        #~(begin
            (use-modules (jrs build npm))

            (npm-fetch
             #$(npm-reference-name ref)
             #$(npm-reference-version ref)
             #$output
             #$(npm-reference-scope ref)
             #:npm-command
             (string-append #+resolved-node "/bin/npm"))))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (npm-reference-name ref) build
                      #:script-name "npm-download"
                      #:system system
                      #:local-build? #t
                      #:hash-algo hash-algo
                      #:hash hash
                      #:guile-for-build guile)))
