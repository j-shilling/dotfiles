;;
;; Special Thanks to https://github.com/nicolas-graves/dotfiles
;;

(when (current-filename)
  (add-to-load-path
   (dirname (current-filename))))

(use-modules
 (guix gexp)
 (ice-9 match)
 (ice-9 pretty-print))

(define (channel-content)
  "Generate the content of the `channels.scm' file."
  `(list
    (channel
     (name 'nonguix)
     (url
      "https://gitlab.com/nonguix/nonguix")
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
    (channel
     (name 'rde)
     (url "https://git.sr.ht/~abcdw/rde")
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
    (channel
     (name 'guix)
     (url "https://git.savannah.gnu.org/git/guix.git")
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define channels-file
  (plain-file
   "channels"
   (with-output-to-string
     (lambda ()
       (pretty-print (channel-content))))))

(use-modules
 (gnu services)
 (gnu services base)
 (gnu services desktop)
 (gnu services ssh)

 (gnu home services)
 (gnu home services mcron)

 (rde features base)

 (jrs config emacs)
 (jrs config mail)
 (jrs config wm)
 (jrs config development)
 (jrs config shell)
 (jrs config profile)
 (jrs config mcron)

 (jrs utils))

(define %main-features
  (append
   %emacs-features
   %desktop-features
   %development-features
   %shell-features
   %mail-features
   (list
    (feature-custom-services
     #:home-services
     (list
      (service home-mcron-service-type
               (home-mcron-configuration
                (jobs %mcron-jobs)))
      (simple-service
       'extend-environment-variables
       home-environment-variables-service-type
       `(("GUILE_LOAD_PATH" . (string-append
                               "${GUILE_LOAD_PATH}:"
                               "${HOME}/.config/guix/current/share/guile/site/3.0"))
         ("XDG_DATA_DIRS" . (string-append
                             "${XDG_DATA_DIRS}:"
                             "/var/lib/flatpak/exports/share:"
                             "${HOME}/.local/share/flatpak/exports/share")))))

     #:system-services
     (if (guix-system?)
         (list
          (simple-service
           'channels-and-sources
           etc-service-type
           `(("channels.scm" ,channels-file)))
          (service guix-publish-service-type
                   (guix-publish-configuration
                    (advertise? #t)
                    (host "0.0.0.0")))
          (service bluetooth-service-type
                   (bluetooth-configuration))
          (service openssh-service-type
                   (openssh-configuration
                    (x11-forwarding? #t)
                    (password-authentication? #f)
                    (authorized-keys
                     `(("jake" ,(local-file "./public-keys/jake.pub")))))))
         (list)))

    (when (guix-system?)
      (feature-base-services
       #:guix-substitute-urls
       (append (list "https://substitutes.nonguix.org")
               (@ (guix store) %default-substitute-urls))
       #:guix-authorized-keys
       (append (list (local-file "./public-keys/nonguix-key.pub"))
               (@ (gnu services base) %default-authorized-guix-keys))))

    (feature-base-packages
     #:home-packages %home-packages
     #:system-packages %system-packages))))

(use-modules
 (rde features)

 (jrs config users)
 (jrs config hosts))

(define-public config
  (rde-config
   (features
    (append
     %user-features
     %host-features
     %main-features))))

(define-public live-config
  (rde-config
   (features
    (append
     %user-features
     %live-host-features
     %main-features))))

(define-public os
  (when (guix-system?)
    (rde-config-operating-system config)))

(define-public home
  (rde-config-home-environment config))

(define-public live-os
  (rde-config-operating-system live-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("home" home)
      ("system" os)
      ("live-system" live-os)
      (_ home))))

(dispatcher)
