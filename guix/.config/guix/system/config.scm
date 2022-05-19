(define-module (system config)
  #:use-module (gnu)
  #:use-module (manifests core)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((keys nonguix) :prefix key:)
  #:export (base-config))

(use-service-modules desktop networking ssh xorg)

(define %modified-desktop-services
  (modify-services %desktop-services
             (guix-service-type config => (guix-configuration
               (inherit config)
               (substitute-urls
                (append (list "https://substitutes.nonguix.org")
                  %default-substitute-urls))
               (authorized-keys
                (append (list key:nonguix)
                        %default-authorized-guix-keys))))
             (gdm-service-type config => (gdm-configuration
                                          (inherit config)
                                          (wayland? #t)))))

(define base-config
  (operating-system

   (locale "en_US.utf8")
   (timezone "America/New_York")
   (keyboard-layout (keyboard-layout "us"))

   (host-name "mercurius")
   (hosts-file
    (plain-file "hosts"
                (string-append
                 (local-host-aliases host-name)
                 "192.168.0.45 olympus\n")))

   (users (cons* (user-account
                  (name "jake")
                  (comment "Jake Shilling")
                  (group "users")
                  (home-directory "/home/jake")
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video")))
                 %base-user-accounts))

   (packages
    (append
     core-packages
     (list emacs-exwm
           sway)
     %base-packages))

   (services
    (append
     (list (service gnome-desktop-service-type)
           (set-xorg-configuration
            (xorg-configuration
             (keyboard-layout keyboard-layout))))
     %modified-desktop-services))

   (bootloader
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets '("/dev/nvme0n1"))
     (keyboard-layout keyboard-layout)))

   (swap-devices
    (list))

   (file-systems
    %base-file-systems)))

(define mercurius-config
  (operating-system
   (inherit base-config)

   (host-name "mercurius")

   (bootloader
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets '("/dev/nvme0n1"))
     (keyboard-layout (keyboard-layout "us"))))

   (swap-devices
    (list
     (swap-space
      (target (uuid "95111f96-068d-4cb7-b02b-1f1252e93d84")))))

   (file-systems
    (cons* (file-system
            (mount-point "/")
            (device
             (uuid "7a00e16d-01e0-4041-aa1e-d2794bdc7b4e"
                   'ext4))
            (type "ext4"))
           %base-file-systems))))

mercurius-config
