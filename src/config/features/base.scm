(define-module (config features base)
  #:use-module (rde features)
  #:use-module (rde features predicates)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  
  #:use-module (guix gexp)
  #:export (feature-foreign-distro))

(define* (feature-foreign-distro
          #:key
          (glibc-locales glibc-locales)
          (nss-certs nss-certs))
  "Provides missing packages and other fixes for rde usage on foreign distro."
  (ensure-pred file-like? glibc-locales)
  (ensure-pred file-like? nss-certs)

  (define (get-home-services _)
    (list
     ;; On ubuntu 20.04 default Guix Home environment fails with
     ;; guile: warning: failed to install locale
     ;; also, some commands fails without nss-certs
     (simple-service
      'foreign-distro-base-packages
      home-profile-service-type
      (list glibc-locales nss-certs))

     ;; The fix for ubuntu, as it doesn't set XCURSOR_PATH, but expects it
     ;; contains a /usr/share/icons if it set.
     (simple-service
      'xcursors-environment-variables-ubuntu-fix
      home-environment-variables-service-type
      `(("XCURSOR_PATH" .
         "/usr/share/icons${XCURSOR_PATH:+:}$XCURSOR_PATH")))

     (simple-service
      'set-nss-certs-path
      home-environment-variables-service-type
      `(("SSL_CERT_DIR" . ,(file-append nss-certs "/etc/ssl/certs"))
        ("SSL_CERT_FILE" . "${GUIX_PROFILE}/etc/ssl/certs/ca-certificates.crt")))))

  (feature
   (name 'foreign-distro)
   (home-services-getter get-home-services)
   (values `((foreign-distro . #t)
             (glibc-locales . ,glibc-locales)
             (nss-certs . ,nss-certs)))))
