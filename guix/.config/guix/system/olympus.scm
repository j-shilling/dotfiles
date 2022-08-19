(define-module (system olympus)
  #:use-module (system base)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:export (olympus-config))

(define olympus-config
  (operating-system
   (inherit base-config)

   (host-name "olympus")
   (keyboard-layout (keyboard-layout "us"))

   (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))

   (swap-devices
    (list (swap-space
            (target
              (uuid "a3ed3f7c-a611-472b-addb-0fa80d450cb1")))))

   (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "F12D-A33C" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "5b3dce06-4eb1-47a3-9b33-f7aa7a2ed83c"
                     'ext4))
             (type "ext4"))
           %base-file-systems))))

olympus-config
