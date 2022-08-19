(define-module (system config)
  #:use-module (gnu)
  #:use-module ((manifests core) :prefix core:)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services docker)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module ((keys nonguix) :prefix key:)
  #:export (base-config))

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
