;; -*- no-byte-compile: t; -*-
;;; desktop/lang/org-private/packages.el

;; (package! org-plus-contrib)
(package! org-brain :recipe
  (:host github
   :repo "Kungsgeten/org-brain"))
(package! org-auto-tangle :recipe
  (:host github
   :repo "yilkalargaw/org-auto-tangle"))
(package! org-ql)
(package! org-appear)
(package! org-download)
(package! org-web-tools)
(package! org-super-agenda)
(package! org-edna)
(package! org-superstar)
(package! org-pdftools)
(package! org-noter-pdftools)
(package! org-sidebar)
(package! org-sticky-header)
(package! org-media-note :recipe
  (:host github
   :repo "yuchen-lea/org-media-note"))
(package! org-expiry :recipe
  (:host github
   :repo "emacs-straight/org-mode"
   :files ("contrib/lisp/org-expiry.el")))
(package! org-pomodoro)
(package! toc-mode)
(package! persistent-scratch)
(package! org-ref)
(package! helm-bibtex)
(package! helm-lib-babel)
(package! powerthesaurus)
(package! ox-reveal)
(package! org-drill)
(package! org-cliplink)
(package! ascii-art-to-unicode)
(package! link-hint)
(package! major-mode-hydra)
(package! abridge-diff)

(package! org-expex :recipe
  (:host gitlab
   :repo "purlupar/org-expex"))
