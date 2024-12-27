(define-module (config features nix)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde serializers ini)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages package-management)
  #:export (feature-nix))

(define* (feature-nix
          #:key
          (nix nix)
          (flakes? #t)
          (extra-config '()))
  (ensure-pred any-package? nix)
  (ensure-pred boolean? flakes?)

  (define (add-nix-conf values)
    `(("nix/nix.conf"
       ,(plain-file
         "nix-conf"
         (apply string-append
                (ini-serialize
                 `((global
                    ((experimental-features . #{nix-command flakes}#)
                     (substituters . #{https://cache.nixos.org https://nix-community.cachix.org}#)
                     (trusted-public-keys . ,(string->symbol
                                              (string-join
                                              '("cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                                                "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=")
                                              " "))))))))))))
  
  (define (get-home-services values)
    (list
     (simple-service 'install-nix-package
                     home-profile-service-type
                     (list nix))
     (simple-service 'home-xdg-nix-configuration-file
                     home-xdg-configuration-files-service-type
                     (add-nix-conf values))))

  (feature
   (name 'nix)
   (values (make-feature-values nix flakes?))
   (home-services-getter get-home-services)))
