;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;(package! builtin-package :recipe (:branch "develop"))
(package! openwith)
(package! pocket-reader)
(package! htmlize)
(package! w3m)
(package! focus)
(package! burly)
(package! undo-tree)
(package! helm-posframe)
(package! ivy-posframe)
(package! ranger)
(package! mpv)
(package! emms)
(package! polymode)
(package! tldr)
(package! guix)
;; (package! emms-helm)
;; (package! emms-org)
;; (package! emms-player-simple-mpv)
(package! subed :recipe
  (:host github
   :repo "rndusr/subed"
   :files ("subed/*.el")))
;; (package! emms-player-mpv-jp-radios)
;; (package! auth-pass)
