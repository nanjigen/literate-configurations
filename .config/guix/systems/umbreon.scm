;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(define-module (umbreon)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "umbreon")

  (mapped-devices
    (list (mapped-device
            (source
              (uuid "602818e7-ce42-4532-a3fb-66c60472db3b"))
            (target "cryptroot")
            (type luks-device-mapping))
          (mapped-device
            (source
              (uuid "06ba3493-7137-4c68-a351-677d0ece80fd"))
            (target "crypthome")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
           (file-system
             (mount-point "/home")
             (device "/dev/mapper/crypthome")
             (type "ext4")
             (dependencies mapped-devices))
           %base-file-systems)))

;; (define %xorg-tearfree-config
;;    "Section \"Device\""
;;    "  Identifier \"Intel Graphics\""
;;    "  Driver \"Intel\""
;;    "  Option \"TearFree\" \"true\""
;;    "EndSection"
;;    "\n")
