;; -*- no-byte-compile: t; -*-
;;; desktop/exwm/packages.el

(package! xelb)
  ;; :recipe (:host github :repo "ch11ng/xelb"))
(package! exwm)
  ;; :recipe (:host github :repo "ch11ng/exwm"))
(package! dbc)
(package! exwm-edit)
(package! dmenu)
(package! posframe)
(package! hydra-posframe :recipe
  (:host github
   :repo "Ladicle/hydra-posframe"))
;; (package! helm-posframe)
;; (package! ivy-posframe)
(package! ace-link)
(package! helm-exwm)
(package! helm-org-rifle)
(package! pulseaudio-control)
(package! desktop-environment)
(package! scrot.el :recipe
  (:host github
   :repo "dakra/scrot.el"
   :files ("*")))
(package! switch-window)
(package! helm-unicode)
(package! exwm-firefox-core)
(package! exwm-firefox-evil)
