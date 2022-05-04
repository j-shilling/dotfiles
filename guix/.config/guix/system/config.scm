;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-service-modules desktop networking ssh xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "mercurius")
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
      (list (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/nvme0n1")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "95111f96-068d-4cb7-b02b-1f1252e93d84")))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "7a00e16d-01e0-4041-aa1e-d2794bdc7b4e"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
