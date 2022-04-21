;;; desktop/exwm/+settings.el -*- lexical-binding: t; -*-

;; variables
;; (defvar exwm-terminal-command "xterm"
;;   "Terminal command to run.")
(defvar exwm-terminal-command "st"
  "Terminal command to run.")

(defvar exwm-locking-command "i3lock"
  "Command to run when locking session")

(defvar exwm-install-logind-lock-handler nil
  "If this is non-nil and `exwm-locking-command' is set, register a D-BUS handler on the session lock signal.")

(defvar exwm-app-launcher--prompt "$ "
  "Prompt for the EXWM application launcher")

(defvar exwm-hide-tiling-modeline nil
  "Whether to hide modeline.")

(defvar exwm-buffer-name-prefix "X:"
  "A prefix to append to each buffer managed by exwm")

;; (defvar exwm-enable-systray t
;;   "Whether to enable EXWM's bundled system tray implementation.")

(defvar exwm-workspace-switch-wrap t
  "Whether `exwm/workspace-next' and `exwm/workspace-prev' should wrap.")
