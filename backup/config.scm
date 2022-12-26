;;
;; Special Thanks to https://github.com/nicolas-graves/dotfiles
;;

(when (current-filename)
  (add-to-load-path
   (dirname (current-filename))))

(use-modules
 (gnu services)
 (gnu services base)
 (gnu services desktop)
 (gnu services networking)
 (gnu services ssh)

 (gnu packages gnome)

 (gnu home services)
 (gnu home services mcron)

 (guix gexp)

 (rde features base)

 (jrs config emacs)
 (jrs config mail)
 (jrs config wm)
 (jrs config development)
 (jrs config shell)
 (jrs config profile)
 (jrs config mcron)
 (jrs config channels)

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
      (simple-service
       'channels-and-sources
       home-xdg-configuration-files-service-type
       `(("guix/channels.scm" ,channels-file)))
      (service home-mcron-service-type
               (home-mcron-configuration
                (jobs %mcron-jobs)))
      (simple-service
       'extend-environment-variables
       home-environment-variables-service-type
       `(("PATH" . (string-append
                    "${PATH}:"
                    "${HOME}/.local/bin"))
         ("GUILE_LOAD_PATH" . (string-append
                               "${GUILE_LOAD_PATH}:"
                               "${HOME}/.config/guix/current/share/guile/site/3.0"))
         ("XDG_DATA_DIRS" . (string-append
                             "${XDG_DATA_DIRS}:"
                             "/var/lib/flatpak/exports/share:"
                             "${HOME}/.local/share/flatpak/exports/share")))))

     #:system-services
     (if (guix-system?)
         (list
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

(use-modules
 (ice-9 match))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("home" home)
      ("system" os)
      ("live-system" live-os)
      (_ home))))

(dispatcher)
