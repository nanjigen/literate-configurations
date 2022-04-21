;;; desktop/exwm/+funcs.el -*- lexical-binding: t; -*-

(use-package! cl-lib)
;; Apparently essentional functions taken from funcs.el spacemacsOS layer

;; Can be used to bind a key to jumping to an application, or alternatively starting it.  E.g.:
;;
;; (exwm/bind-switch-to-or-run-command "s-f" "Firefox" "firefox")
;;
;; The window class can be found out with exwm's builtin info functions, but for most applications
;; it should just match the buffer name.
(defun exwm/bind-switch-to-or-run-command (key window-class command)
  (exwm-input-set-key (kbd key)
                      `(lambda ()
                         (interactive)
                         (exwm/switch-to-buffer-or-run ,window-class ,command))))

;; (defun exwm//switch-to-line-mode ()
;;   "Used as a hook to switch to line mode when transient mode starts."
;;   (when (eq exwm--input-mode 'char-mode)
;;     ;; (setq exwm--switch-to-char-after-transient (current-buffer))
;;     (call-interactively 'exwm-input-grab-keyboard)))

(defun exwm//persp-mode-inhibit-p (frame)
  (frame-parameter frame 'unsplittable))

(defun exwm/bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

;; Simulate insert state by using line mode without passthrough
(defun exwm/enter-insert-state ()
  (interactive)
  (setq exwm-input-line-mode-passthrough nil)
  (call-interactively 'exwm-input-grab-keyboard)
  (evil-insert-state))

;; Simulate normal state by using line mode with passthrough, i.e. forward all commands to emacs
(defun exwm/enter-normal-state ()
  (interactive)
  (setq exwm-input-line-mode-passthrough t)
  (call-interactively 'exwm-input-grab-keyboard)
  (evil-normal-state))

(defun exwm/escape ()
  "Switch to normal state, and cancel possible fullscreen layout.  Also close minibuffer."
  (interactive)
  (exwm/enter-normal-state)
  (exwm-layout-unset-fullscreen)
  (when (active-minibuffer-window)
    (minibuffer-keyboard-quit)))

(defun exwm/enter-char-mode ()
  "Enter EXWM char mode."
  (interactive)
  (when exwm--id
    (exwm/enter-insert-state)
    (call-interactively 'exwm-input-release-keyboard)))

(defun exwm/switch-to-buffer-or-run (window-class command)
  "Switch to first buffer with window-class, and if not present, run command."
  (let ((buffer
         (cl-find window-class (buffer-list) :key (lambda(b) (cdr (assoc 'exwm-class-name (buffer-local-variables b)))) :test 'string-equal)))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (start-process-shell-command command nil command))))

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(defun exwm/rename-buffer ()
  (let* ((part1 exwm-class-name)
         (part2 (when (not (string-equal exwm-class-name exwm-title))
                  (concat "/" exwm-title)))
         (name (concat exwm-buffer-name-prefix part1 (or part2 "")))
         (maxlen 40))
    (exwm-workspace-rename-buffer (if (> (length name) maxlen)
                                      (concat (cl-subseq name 0 (- maxlen 3)) "...")
                                    name))))

;; (add-hook 'exwm-update-class-hook
;;           (lambda ()
;;            (unless (string= "sm18.exe" exwm-instance-name))
;;               (exwm-workspace-rename-buffer exwm-class-name)))
;; (add-hook 'exwm-update-title-hook
;;           (lambda ()
;;             (when (not exwm-instance-name)
;;               (string= "sm18.exe" exwm-instance-name))
;;             (exwm-workspace-rename-buffer exwm-title)))

;; (setq exwm-manage-configurations
;;      '(((equal exwm-instance-name "sm18.exe")
;;          managed t exwm-manage-force-tiling t)))

; test workspace management
;; (use-package! xelb
;; :after exwm
;; :init
;; (setq exwm-workspace-index-map
;;         (lambda (index)
;;           (let ((named-workspaces ["code" "brow" "extr" "slac" "lisp" "test" "seven"]))
;;             (if (< index (length named-workspaces))
;;                 (elt named-workspaces index)
;;               (number-to-string index)))))
;;
;; (defun exwm-workspace--update-ewmh-desktop-names ()
;;   (xcb:+request exwm--connection
;;       (make-instance 'xcb:ewmh:set-_NET_DESKTOP_NAMES
;;                      :window exwm--root :data
;;                      (mapconcat (lambda (i) (funcall exwm-workspace-index-map i))
;;                                 (number-sequence 0 (1- (exwm-workspace--count)))
;;                                 "\0"))))
;;
;; (add-hook 'exwm-workspace-list-change-hook
;;           #'exwm-workspace--update-ewmh-desktop-names)
;;
;; ;; you may need to call the function once manually
;; (exwm-workspace--update-ewmh-desktop-names)
;; )
;;

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "Firefox"))
              ;; (exwm-input-set-local-simulation-keys
              ;;  '(([?\C-b] . left)
              ;;    ([?\C-f] . right)))
              (exwm-input-set-local-simulation-keys nil))))

;; testing funcs
(defun exwm/layout-toggle-fullscreen ()
  "Toggles full screen for Emacs and X windows"
  (interactive)
  (if exwm--id
      (if (exwm-layout--fullscreen-p)
          (exwm-reset)
        (exwm-layout-set-fullscreen))
    (spacemacs/toggle-maximize-buffer)))

(defun exwm/run-program-in-home (command)
  (let ((default-directory user-home-directory))
    (start-process-shell-command command nil command)))

(defun exwm/app-launcher (command)
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ivy"
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (exwm/run-program-in-home command))

(defun exwm/launch-split-below (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-below-and-focus)
  (exwm/run-program-in-home command))

(defun exwm/launch-split-right (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-right-and-focus)
  (exwm/run-program-in-home command))

(defun exwm/jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm-toggle-workspace))

(defun exwm/exwm-buffers-info ()
  "Helper, return information about open exwm windows"
  (cl-loop for buffer in (buffer-list)
        for name = (buffer-name buffer)
        for ecname = (buffer-local-value 'exwm-class-name buffer)
        when ecname
        collect (list :buffer-name name :exwm-class-name ecname)))
