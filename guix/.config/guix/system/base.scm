(define-module (system base)
  #:use-module (gnu)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services docker)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services syncthing)
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
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))

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
                   '("wheel" "netdev" "audio" "lp" "video" "docker")))
                 %base-user-accounts))

   (packages
    (append
     ;core:packages
     (list sway
           i3-gaps)
     %base-packages))

   (services
    (append
     (list (service gnome-desktop-service-type)
           (set-xorg-configuration
            (xorg-configuration
             (keyboard-layout keyboard-layout)))
           (service docker-service-type)
           (service syncthing-service-type
                    (syncthing-configuration (user "jake")))
           (service openssh-service-type
                    (openssh-configuration
                     (authorized-keys
                      `(("jake" ,(local-file "../keys/jake.pub"))))))
           (service guix-publish-service-type
                    (guix-publish-configuration
                     (advertise? #t)))
           (service bluetooth-service-type
                    (bluetooth-configuration)))
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
