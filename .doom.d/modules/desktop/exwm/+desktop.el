(defun nanjigen/run-xmodmap ()
  (interactive)
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.dotfiles/.config/i3/Xmodmap"))

(defun nanjigen/update-wallpapers ()
  (interactive)
  (start-process-shell-command
   "feh" nil
   (format "feh --bg-scale ~/.dotfiles/backgrounds/%s" (alist-get 'desktop/background nanjigen/system-settings))))

(setq nanjigen/panel-process nil)
(defun nanjigen/kill-panel ()
  (interactive)
  (when nanjigen/panel-process
    (ignore-errors
      (kill-process nanjigen/panel-process)))
  (setq nanjigen/panel-process nil))

(defun nanjigen/start-panel ()
  (interactive)
  (nanjigen/kill-panel)
  (setq nanjigen/panel-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun nanjigen/update-screen-layout ()
  (interactive)
  (let ((layout-script "~/.bin/update-screens"))
     (message "Running screen layout script: %s" layout-script)
     (start-process-shell-command "xrandr" nil layout-script)))

(defun nanjigen/configure-desktop ()
  (interactive)
    (nanjigen/run-xmodmap)
    (nanjigen/update-screen-layout)
    (run-at-time "2 sec" nil (lambda () (nanjigen/update-wallpapers))))

(defun nanjigen/on-exwm-init ()
  (nanjigen/configure-desktop)
  (nanjigen/start-panel))

;; (when nanjigen/exwm-enabled
;;   ;; Configure the desktop for first load
;;   (add-hook 'exwm-init-hook #'nanjigen/on-exwm-init))

;;; desktop/exwm/+desktop.el -*- lexical-binding: t; -*-

(defun nanjigen/send-polybar-hook (name number)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" name number)))

(defun nanjigen/update-polybar-exwm (&optional path)
  (nanjigen/send-polybar-hook "exwm" 1)
  (nanjigen/send-polybar-hook "exwm-path" 1))

;; (defun nanjigen/update-polybar-telegram ()
;;   (nanjigen/send-polybar-hook "telegram" 1))

(defun nanjigen/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun nanjigen/polybar-exwm-workspace-path ()
  (let ((workspace-path (frame-parameter nil 'bufler-workspace-path-formatted)))
    (if workspace-path
        (substring-no-properties workspace-path)
      "")))

(defun nanjigen/polybar-mail-count (max-count)
  (if (and nanjigen/mail-enabled nanjigen/mu4e-inbox-query)
    (let* ((mail-count (shell-command-to-string
                         (format "mu find --nocolor -n %s \"%s\" | wc -l" max-count nanjigen/mu4e-inbox-query))))
      (format " %s" (string-trim mail-count)))
    ""))

(defun nanjigen/telega-normalize-name (chat-name)
  (let* ((trimmed-name (string-trim-left (string-trim-right chat-name "}") "◀{"))
         (first-name (nth 0 (split-string trimmed-name " "))))
    first-name))

(defun nanjigen/propertized-to-polybar (buffer-name)
  (if-let* ((text (substring-no-properties buffer-name))
            (fg-face (get-text-property 0 'face buffer-name))
            (fg-color (face-attribute fg-face :foreground)))
    (format "%%{F%s}%s%%{F-}" fg-color (nanjigen/telega-normalize-name text))
    text))

;; (defun nanjigen/polybar-telegram-chats ()
;;   (if (> (length tracking-buffers) 0)
;;     (format " %s" (string-join (mapcar 'nanjigen/propertized-to-polybar tracking-buffers) ", "))
;;     ""))

(add-hook 'exwm-workspace-switch-hook #'nanjigen/update-polybar-exwm)
(add-hook 'bufler-workspace-set-hook #'nanjigen/update-polybar-exwm)
