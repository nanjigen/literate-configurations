;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
(load! "+funcs")
(use-package! xelb)
(use-package! exwm
  :init
  (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)

  (load! "+desktop")
  ;; (load! "+polybar")
  (setq exwm-workspace-number 7)
  ;;    (when exwm-hide-tiling-modeline
                                        ;(add-hook 'exwm-mode-hook #'hidden-mode-line-mode))
  (setq exwm-input-line-mode-passthrough t)

  (load! "+settings")

    :config
  
  ;; 
  
  ;; 
  
                                            ;    (add-hook 'exwm-update-class-hook 'exwm/rename-buffer)
                                            ;    (add-hook 'exwm-update-title-hook 'exwm/rename-buffer)
  
    ;; kick all exwm buffers into insert mode per default
    (add-hook 'exwm-manage-finish-hook 'exwm/enter-insert-state)
  
    ;; Quick swtiching between workspaces
    (defvar exwm-toggle-workspace 0
      "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")
  
    ;; Buffer switching settings:
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)
  
    (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
      (setq exwm-toggle-workspace exwm-workspace-current-index))
  
    ;; `exwm-input-set-key' sets global key bindings, independent of char mode, line mode, and line mode passthru
  
    ;; + We always need a way to get to normal state if we are in insert state.
    (exwm-input-set-key (kbd "s-<escape>") 'exwm/escape)
  
    (exwm-input-set-key (kbd "<s-tab>") #'exwm/jump-to-last-exwm)
    ;; + Set shortcuts to switch to a certain workspace.
    (exwm-input-set-key (kbd "s-1")
                        (lambda () (interactive) (exwm-workspace-switch 0)))
    (exwm-input-set-key (kbd "s-2")
                        (lambda () (interactive) (exwm-workspace-switch 1)))
    (exwm-input-set-key (kbd "s-3")
                        (lambda () (interactive) (exwm-workspace-switch 2)))
    (exwm-input-set-key (kbd "s-4")
                        (lambda () (interactive) (exwm-workspace-switch 3)))
    (exwm-input-set-key (kbd "s-5")
                        (lambda () (interactive) (exwm-workspace-switch 4)))
    (exwm-input-set-key (kbd "s-6")
                        (lambda () (interactive) (exwm-workspace-switch 5)))
    (exwm-input-set-key (kbd "s-7")
                        (lambda () (interactive) (exwm-workspace-switch 6)))
    (exwm-input-set-key (kbd "s-8")
                        (lambda () (interactive) (exwm-workspace-switch 7)))
    (exwm-input-set-key (kbd "s-9")
                        (lambda () (interactive) (exwm-workspace-switch 8)))
    (exwm-input-set-key (kbd "s-0")
                        (lambda () (interactive) (exwm-workspace-switch 9)))
  
    ;; in normal state/line mode, use the familiar i key to switch to input state
    ;;  (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
    ;; (evil-define-key 'normal exwm-mode-map
  (after! exwm
    (map! :map exwm-mode-map
          :n "i" #'exwm/enter-insert-state))
    ;; (push ?\i exwm-input-prefix-keys)
    ;; (push ?\  exwm-input-prefix-keys)
    ;;      (kbd "i") #'exwm/enter-insert-state)
    ;; (map! :map exwm-mode-map
    ;;       :n "i" #'exwm-input-release-keyboard)
  
  (setq exwm-workspace-warp-cursor t)

    ;;; Some programs escape EXWM control and need be tamed.  See
    ;; https://github.com/ch11ng/exwm/issues/287
    ;; (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "sm18.exe") managed t floating nil))
    (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "TotalWarhammer2")
                                               managed t floating nil fullscreen t char-mode t))
  
    (load! "+sm-window-rules")
    (load! "+sm-emacs-protocol")
    (load! "+exwm-sm-core")
    (load! "+exwm-sm-evil")
    (load! "+sm-hydra")
    ;; FIXME See if I can get this randr code working without affecting the above.
    (use-package! exwm-randr
      :config
      (setq exwm-randr-workspace-output-plist '(0 "HDMI-A-0" 1 "DisplayPort-0"))
      ;; (setq exwm-monitor-list '("HDMI1" "DP2"))
      ;; https://github.com/ch11ng/exwm/issues/202#issuecomment-559222831
      ;; (setq exwm-workspace-name-alist '((0 . "Dashboard")
      ;;                                   (1 . "Code")
      ;;                                   (2 . "Comms")
      ;;                                   (3 . "Translation")
      ;;                                   (4 . "Study")
      ;;                                   (5 . "Reading")
      ;;                                   (6 . "Extra")))
  
      ;; (setq exwm-workspace-monitor-alist '(("Dashboard" . "HDMI1")
      ;;                                      ("Code" . "HDMI1")
      ;;                                      ("Comms" . "HDMI1")
      ;;                                      ("Translation" . "HDMI1")
      ;;                                      ("Study" . "HDMI1")
      ;;                                      ("Reading" . "DP2")
      ;;                                      ("Extra" . "DP2")))
  
      ;; assign programs to workspaces
      ;; https://emacs.stackexchange.com/questions/33107/in-exwm-emacs-x-window-manager-how-can-i-assign-apps-to-particular-workspaces
      ;; (setq exwm-manage-configurations
      ;;       '(((equal exwm-class-name "Anki")
      ;;          workspace (car (rassoc "Study" exwm-workspace-name-alist)))))
  
      ;; (setq exwm-manage-configurations
      ;;       '(((equal exwm-class-name "Anki")
      ;;          workspace 4)))
  
      ;; (defun update-exwm-randr-workspace-monitor-plist ()
      ;;       "Update exwm-randr-workspace-monitor-plist based on the current
      ;;        value of exwm-workspace-monitor-alist"
      ;;       (setq exwm-randr-workspace-monitor-plist (mapcan (lambda (workspace->monitor)
      ;;                                                          (let ((workspace-number (car (rassoc (car workspace->monitor)
      ;;                                                                                                exwm-workspace-name-alist)))
      ;;                                                                 (monitor (cdr workspace->monitor)))
      ;;                                                            (list workspace-number monitor)))
      ;;                                                        exwm-workspace-monitor-alist)))
      ;; (update-exwm-randr-workspace-monitor-plist)
  
      (add-hook 'exwm-randr-screen-change-hook
                (lambda ()
                  (start-process-shell-command
                   ;; "xrandr" nil "xrandr --output HDMI1 --primary --mode 1920x1080_60.00 --pos 900x0 --rotate normal")))
                   "xrandr" nil "xrandr --output HDMI-A-0 --primary --mode 1920x1080 --rotate normal --output DisplayPort-0 --left-of HDMI-A-0 --mode 1440x900 --rotate left"))))
  
    ;; (setq exwm-randr-workspace-monitor-plist '())
  
    (exwm-randr-enable)
  (use-package! hydra-posframe
    :hook (after-init . hydra-posframe-enable))
  
  (use-package! posframe)
  
  (with-eval-after-load 'posframe
      (define-advice posframe-show (:filter-return (frame) exwm-deparent)
        (set-frame-parameter frame 'parent-frame nil)
        frame))

  ;; (use-package! exwm-firefox-evil
  ;;   :defer t
  ;;   :config
  ;;   (add-to-list 'exwm-firefox-evil-firefox-class-name "firefox")
  ;;   (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox))
  
  ;; (use-package! +exwm-sm-evil
  ;; (add-hook 'exwm-manage-finish-hook 'exwm-sm-evil-activate-if-sm))
  
  ;; Define super-space as default leader key.
  (exwm-input-set-key (kbd "s-SPC") doom-leader-map)
  
  (defun org-media-note-hydra-based ()
    "load hydra in minibuffer"
    (interactive)
    (hydra-posframe-mode 0)
    (org-media-note-hydra/body))
  
  
  ;; TODO clean this up:
  ;; EXWM does not bypass exwm-mode-map keybindings in line-mode, so the
  ;; default bindings are still mapped to C-c.  We remap that to C-s-c.
  
  (exwm-input-set-key (kbd "s-n") #'org-media-note-hydra-based)
  (exwm-input-set-key (kbd "s-g") #'guix)
  ;; (exwm-input-set-key (kbd "s-n") #'org-media-note-hydra/body)
  ;; (exwm-input-set-key (kbd "s-N") #'org-journal-new-entry)
  ;; (exwm-input-set-key (kbd "s-N") #'elfeed)
  ;; (exwm-input-set-key (kbd "s-W") #'nmtui)
  (exwm-input-set-key (kbd "s-v") #'exwm-floating-toggle-floating)
  (exwm-input-set-key (kbd "s-Q") #'kill-buffer-and-window)
  (exwm-input-set-key (kbd "s-q") #'kill-this-buffer)
  ;; (exwm-input-set-key (kbd "s-<down-mouse-1>") #'exwm-input-move-event)
  (exwm-input-set-key (kbd "s-e") #'mu4e)
  (exwm-input-set-key (kbd "s-o") #'link-hint-open-link)
  ;; TODO possibly have N for journal entries and another n for somekind of popup/dropdown notetaking
  ;; TODO replace with ``consult-info'
  (exwm-input-set-key (kbd "s-<f1>") #'helm-info)
  ;; (exwm-input-set-key (kbd "s-<f4>") #'wttrin)
  ;; (exwm-input-set-key (kbd "s-<f6>") #'transmission)
  ;; (exwm-input-set-key (kbd "s-<f8>") #'scrot)
  ;; (exwm-input-set-key (kbd "s-m") #'major-mode-hydra)
  ;; (define-key exwm-mode-map (kbd "s-v") #'exwm-floating-toggle-floating)
  ;; (exwm-input-set-key (kbd "s-:") 'helm-M-x)
  ;; (exwm-input-set-key (kbd "s-;") 'evil-ex)
  ;; (exwm-input-set-key (kbd "s-g") 'bookmark-jump)
  (exwm-input-set-key (kbd "s-d") #'counsel-linux-app)
  (exwm-input-set-key (kbd "s-y") #'org-agenda)
  (exwm-input-set-key (kbd "s-a") #'calc)
  ;; (exwm-input-set-key (kbd "s-I") #'launch-htop)
  ;; TODO pressing =s-r= again doesn't exit ranger-mode
  (exwm-input-set-key (kbd "s-r") #'ranger)
  (exwm-input-set-key (kbd "s-c") #'org-capture)
  ;; (global-set-key (kbd "s-w") #'exwm/app-launcher ('firefox))
  (use-package! desktop-environment
      :after exwm
      :config (desktop-environment-mode)
      :custom
      (desktop-environment-brightness-small-increment "2%+")
      (desktop-environment-brightness-small-decrement "2%-")
      (desktop-environment-brightness-normal-increment "5%+")
      (desktop-environment-brightness-normal-decrement "5%-"))
  
  (exwm-input-set-key (kbd "s--") #'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "s-=") #'desktop-environment-volume-increment-slowly)
  (exwm-input-set-key (kbd "s-0") #'desktop-environment-toggle-mute)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'desktop-environment-toggle-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment-slowly)
  (exwm-input-set-key (kbd "<XF86AudioPrev>") #'desktop-environment-music-previous)
  (exwm-input-set-key (kbd "<XF86AudioNext>") #'desktop-environment-music-next)
  (exwm-input-set-key (kbd "<XF86AudioStop>") #'desktop-environment-music-stop)
  (exwm-input-set-key (kbd "<XF86AudioPlay>") #'desktop-environment-toggle-music)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'desktop-environment-brightness-decrement)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'desktop-environment-brightness-increment)
  ;;   (use-package xbacklight
  ;; :bind (("<XF86MonBrightnessUp>" . xbacklight-increase)
  ;;        ("<XF86MonBrightnessDown>" . xbacklight-decrease)))
  
    ;; Pop ups
    ;; (exwm-input-set-key (kbd "s-\\") #'helm-org-brain)
    (exwm-input-set-key (kbd "s-?") #'helm-org-rifle)
    (exwm-input-set-key (kbd "s-|") #'consult-bibtex)
    (exwm-input-set-key (kbd "s-u") #'+eshell/toggle)
    (exwm-input-set-key (kbd "s-`") #'+popup/toggle)
    (exwm-input-set-key (kbd "s-;") #'+popup-toggle-brain)
    ;; Change buffers
    (exwm-input-set-key (kbd "s-b") #'consult-buffer) ;; try excluding EXWM buffers
    ;; (exwm-input-set-key (kbd "s-B") #'helm-exwm)
  (defun nanjigen/lockscreen ()
    (interactive)
    "simple shell call for locking screen"
    ;; TODO This needs to be changed to use i3lock
    (shell-command "betterlockscreen -l --display 1"))
  
  ;; TODO pretty-hydra icons
  ;; (defvar logoff-menu--title (with-faicon ))
  
  (pretty-hydra-define logoff-menu (:foreign-keys warn :title "logoff" :color blue :quit-key "q")
    ("Actions"
     (("l" nanjigen/lockscreen "lock computer")
      ("s" nanjigen/shutdown "shutdown computer")
      ("m" nanjigen/restart "restart computer")
      ("Q" save-buffers-kill-terminal "exit Emacs and logout"))))
  
    (exwm-input-set-key (kbd "s-x") #'logoff-menu/body)
  
    ;; Pop ups
    (exwm-input-set-key (kbd "s-\\") #'helm-org-brain)
    (exwm-input-set-key (kbd "s-?") #'helm-org-rifle)
    (exwm-input-set-key (kbd "s-|") #'helm-bibtex)
    (exwm-input-set-key (kbd "s-u") #'+eshell/toggle)
    (exwm-input-set-key (kbd "s-`") #'+popup/toggle)
    (exwm-input-set-key (kbd "s-;") #'+popup-toggle-brain)
    ;; Change buffers
    (exwm-input-set-key (kbd "s-b") #'helm-mini) ;; try excluding EXWM buffers
    (exwm-input-set-key (kbd "s-B") #'helm-exwm)
    ;; Focusing windows
    (exwm-input-set-key (kbd "s-h") #'evil-window-left)
    (exwm-input-set-key (kbd "s-j") #'evil-window-down)
    (exwm-input-set-key (kbd "s-k") #'evil-window-up)
    (exwm-input-set-key (kbd "s-l") #'evil-window-right)
    (exwm-input-set-key (kbd "s-\/") #'split-window-below)
    (exwm-input-set-key (kbd "s-<return>") #'split-window-right)
    ;; Moving Windows
    (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
    (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
    (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
    (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
    ;; Resize
    (exwm-input-set-key (kbd "s-Y") #'enlarge-window-horizontally)
    (exwm-input-set-key (kbd "s-U") #'shrink-window)
    (exwm-input-set-key (kbd "s-I") #'enlarge-window)
    (exwm-input-set-key (kbd "s-O") #'shrink-window-horizontally)
  
  (use-package! switch-window
    :after exwm
    (setq switch-window-input-style 'minibuffer
          switch-window-shortcut-style 'qwerty))
  
  (defun nanjigen/min-max ()
    (interactive)
    (if (< 1 (count-windows))
        (doom/window-maximize-buffer)
      (winner-undo)))
  
  (exwm-input-set-key (kbd "s-f") #'nanjigen/min-max)
  
  
  ;; Workspaces
  (exwm-input-set-key (kbd "s-]") #'next-buffer)
  (exwm-input-set-key (kbd "s-[") #'previous-buffer)
  
  (exwm-input-set-key (kbd "s-'") #'helm-bookmarks)
  ;; (exwm-input-set-key (kbd "s-i") #'exwm-edit--compose)
  ;; Window management
    (use-package! ace-window
      :init
      (setq ace-window-display-mode t))
  

   (use-package! exwm-edit
     :after exwm
     :custom
     (exwm-edit-bind-default-keys nil)
  
     :config
     (defalias 'exwm-edit--display-buffer 'pop-to-buffer)
     (defun exwm/on-exwm-edit-compose ()
       ;; (spacemacs/toggle-visual-line-navigation-on)
       (funcall 'org-mode))
     ;; include frame height restrictions here?
     (add-hook 'exwm-edit-compose-hook 'exwm/on-exwm-edit-compose))

  (nanjigen/start-panel)

  (exwm-enable))
