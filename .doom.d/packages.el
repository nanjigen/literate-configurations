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
;; (package! helm-posframe)
(package! counsel) ;; for `counsel-linux-app'
(package! ivy-posframe)
(package! lispy)
(package! lispyville)
(package! ranger)
(package! mpv)
(package! emms)
(package! polymode)
(package! tldr)
(package! nix-mode)
(package! sly :pin "ea83bbf0b3e1a20ab172fde42a196b7b8cf0812c")
(package! poke-line :recipe
  (:host github
   :repo "RyanMillerC/poke-line"))
;; (package! emms-helm)
;; (package! emms-org)
;; (package! emms-player-simple-mpv)
(package! subed :recipe
  (:host github
   :repo "rndusr/subed"
   :files ("subed/*.el")))
(package! bookmark+ :recipe
  (:host github
   :repo "emacsmirror/bookmark-plus"))
;; (package! emms-player-mpv-jp-radios)
