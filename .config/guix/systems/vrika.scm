;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(define-module (vrika)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "vrika")

 (mapped-devices
  (list (mapped-device
         (source (uuid "6b665aab-a741-41bf-be36-c4c856d15752"))
         (target "system-root")
         (type luks-device-mapping))))

 (file-systems (cons*
                (file-system
                 (device (file-system-label "vrika"))
                 (mount-point "/")
                 (type "ext4")
                 (dependencies mapped-devices))
                (file-system
                 (device "/dev/nvme0n1p1")
                 (mount-point "/boot/efi")
                 (type "vfat"))
                %base-file-systems)))
