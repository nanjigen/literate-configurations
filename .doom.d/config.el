;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; TODO having issues with pass and 'no secrect key'
;; (setq user-full-name "Daniel"
;;       user-mail-address (auth-source-pass-get "user" "email/personal/hotmail"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

(setq doom-font (font-spec :family "JetBrainsMono" :size 14))
;; (setq doom-font (font-spec :family "Ubunto Mono" :size 13))
;; doom-variable-pitch-font (font-spec :family "Ubuntu Mono" :size 13)
;; doom-big-font (font-spec :family "Ubunto Mono" :size 15))
;; (after! doom-themes
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t))
;; (custom-set-faces
;;  '((font-lock-comment-face :)))

(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; (map! "<f8>" #'scrot)
(setq display-battery-mode nil)


(load-file "~/.doom.d/per-system-settings.el") ;; TODO Not sure if this does anything here

(setq org-directory "~/org/")
(setq user-home-directory "~/")
(setq ispell-dictionary "en")

;; Bookmarks file location
(setq bookmark-default-file "~/org/bookmarks")
(setq bookmark-save-flag 1) ;; save after every change

(use-package! bookmark+
  :after bookmark)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setq scrot-local-path "~/Pictures/screenshots")
(setq eww-download-directory "~/Downloads/eww")

(setq evil-escape-mode nil)

(setq tab-bar-mode t)
(setq doom-modeline-continuous-word-count-modes '(Tex-Pdf markdown-mode org-mode))

(defconst doom-frame-transparency 94)
(set-frame-parameter (selected-frame) 'alpha doom-frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,doom-frame-transparency))

(use-package! poke-line
  :config
  (poke-line-global-mode 1)
  (setq-default poke-line-pokemon "gengar"))

;; If this isn't set EXWM quadruples its memory usage
(setq gc-cons-threshold 100000000)
;; Data read from process - default too low for lsp
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; TODO lsp enhancements:
;; (setq lsp-idle-delay 0.500
;;       lsp-log-io nil) ; if set to true can cause a performance hit

(use-package! undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.doom.d/undo"))))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-undo-system 'undo-tree
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(setq +zen-text-scale 0.6)

(after! writeroom-mode
  (add-hook 'writeroom-mode-hook
            (defun +zen-cleaner-org ()
              (when (and (eq major-mode 'org-mode) writeroom-mode)
                (setq-local -display-line-numbers display-line-numbers
                            display-line-numbers nil)
                (setq-local -org-indent-mode org-indent-mode)
                (org-indent-mode -1))))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-dirty-org ()
              (when (eq major-mode 'org-mode)
                (setq-local display-line-numbers -display-line-numbers)
                (when -org-indent-mode
                  (org-indent-mode 1))))))

(use-package! focus
  :after writeroom-mode
  :config
  (add-to-list 'focus-mode-to-thing '(writeroom-mode . paragraph)))
;; (add-hook 'write-room-mode-hook #'line-number-mode-hook)

(load! "+mail")

(setq nanjigen/mail-enabled (member system-name '("umbreon" "espeon")))
(setq nanjigen/mu4e-inbox-query nil)

;; File handling
(use-package! openwith
  :config
  (setq openwith-associations (list
                               (list (openwith-make-extension-regexp
                                 '("mpg" "mpeg" "mp3" "mp4" "m4v"
                                   "avi" "wmv" "wav" "mov" "flv"
                                   "ogm" "ogg" "mkv" "webm" "webp"))
                                "mpv"
                                '(file))

                               (list (openwith-make-extension-regexp
                                      '("odt"))
                                     "libreoffice"
                                     '(file))))
  (openwith-mode 1))

;; Advise to not warn when opening `openwith-associations' videos
;; https://emacs.stackexchange.com/a/62234
(define-advice abort-if-file-too-large
    (:around (orig-fn size op-type filename &optional offer-raw) unless-openwith-handles-it)
  "Do not abort if FILENAME is handled by Openwith."
  (let ((my-ok-large-file-types (mapconcat 'car openwith-associations "\\|")))
    (unless (string-match-p my-ok-large-file-types filename)
      (funcall orig-fn size op-type filename offer-raw))))

  (use-package! subed
    :config
    (add-hook 'subed-mode-hook 'save-place-local-mode))

  (use-package! mpv
    :defer-incrementally t
    :config
    (org-add-link-type "mpv" #'mpv-play)
    (defun org-mpv-complete-link (&optional arg)
      (replace-regexp-in-string
       "file:" "mpv:"
       (org-file-complete-link arg)
       t t)))

  (defun my-fetch-password (&rest params)
    (require 'auth-source)
    (let ((match (car (apply #'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))
        (error "Password not found for %S" params))))

(after! circe
  (set-irc-server! "irc.libera.chat"
                   `(:tls t
                     :port 6697
                     :nick "nanjigen"
                     :sasl-username ,(+pass-get-user "irc/libera.chat")
                     :sasl-password (lambda (&rest _) (+pass-get-secret "comms/irc"))
                     :channels ("#emacs" "#lisp" "#lispgames" "#guix" "#japanese"))))

(after! tramp
    ;; Make sure we work on remote guixsd machines :)
    ;; probably only helps if you start on a guixsd machine..!
    (setq tramp-remote-path
          (append tramp-remote-path
                  '(tramp-own-remote-path
                    "~/.guix-profile/bin" "~/.guix-profile/sbin"))))

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages
     (quote
      (focus darkroom symbol-navigation-hydra org-sidebar link-hint))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
