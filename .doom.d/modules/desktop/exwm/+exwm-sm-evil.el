;;; desktop/exwm/+exwm-sm-evil.el -*- lexical-binding: t; -*-

(require 'evil)
(require 'evil-core)
(require '+exwm-sm-core)

(defvar exwm-sm-evil-sm-class-name '("sm18.exe")
  "The class name use for detecting if a SM buffer is selected.")

;; (defvar exwm-sm-evil-sm-buffer-name '(sm-element-window)
;;   "The buffer name used for detecting if a SM buffer is selected.")

;;; State transitions
(defun exwm-sm-evil-normal ()
  "Pass every key directly to Emacs."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t)
  (evil-normal-state))

(defun exwm-sm-evil-insert ()
  "Pass every key to SM."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough nil)
  (evil-insert-state))

(defun exwm-sm-evil-visual ()
  "Visual mode!"
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t)
  (shell-command
   (format "xdotool keydown Shift sleep 0.1"))
  (evil-visual-state))

(defun exwm-sm-evil-exit-visual ()
  "Exit visual state properly."
  (interactive)
  ;; Unmark any selection
  (shell-command
   (format "xdotool keyup Shift"))
  (exwm-sm-core-left)
  (exwm-sm-core-right)
  (exwm-sm-evil-normal))

(defun exwm-sm-evil-visual-change ()
  "Change text in visual mode."
  (interactive)
  (exwm-sm-core-cut)
  (exwm-sm-evil-insert))

;;; Keys
(defvar exwm-sm-evil-mode-map (make-sparse-keymap))

    ;;;; Transitions
;; Bind normal
(define-key exwm-sm-evil-mode-map [remap evil-exit-visual-state] 'exwm-sm-evil-exit-visual)
(define-key exwm-sm-evil-mode-map [remap evil-normal-state] 'exwm-sm-evil-normal)
(define-key exwm-sm-evil-mode-map [remap evil-force-normal-state] 'exwm-sm-evil-normal)
(define-key exwm-sm-evil-mode-map [remap evil-visual-state] 'exwm-sm-evil-visual)
;; Bind insert
(define-key exwm-sm-evil-mode-map [remap evil-insert-state] 'exwm-sm-evil-insert)
(define-key exwm-sm-evil-mode-map [remap evil-insert] 'exwm-sm-evil-insert)
(define-key exwm-sm-evil-mode-map [remap evil-substitute] 'exwm-sm-evil-insert)
(define-key exwm-sm-evil-mode-map [remap evil-append] 'exwm-sm-evil-insert)

;;;; Normal
;; Basic movements

(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "w") 'exwm-sm-core-forward-word-test)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "k") 'exwm-sm-core-up)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "j") 'exwm-sm-core-down)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "h") 'exwm-sm-core-left)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "l") 'exwm-sm-core-right)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "K") 'exwm-sm-core-goto-parent)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "J") 'exwm-sm-core-goto-child)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "H") 'exwm-sm-core-back)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "L") 'exwm-sm-core-forward)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "w") 'exwm-sm-core-word)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "b") 'exwm-sm-core-word-back)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "0") 'exwm-sm-core-beginning-of-line)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "$") 'exwm-sm-core-end-of-line)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "/") 'exwm-sm-core-find)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "t") 'exwm-sm-core-test)
;; (evil-define-key 'normal exwm-sm-evil-mode-map (kbd "<escape>") 'exwm-sm-core-escape)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "<return>") '(lambda () (interactive) (exwm-input--fake-key 'return)))
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "RET") '(lambda () (interactive) (exwm-input--fake-key 'return)))

;;; Motion State
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "gg") 'exwm-sm-core-goto-first-line)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "G") 'exwm-sm-core-goto-last-line)

(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "gq") 'exwm-sm-core-edit-question)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "ga") 'exwm-sm-core-edit-answer)

(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "C-u") 'exwm-sm-core-scroll-up)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "C-d") 'exwm-sm-core-scroll-up)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "C-b") 'exwm-sm-core-scroll-page-up)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "C-e") 'exwm-sm-core-scroll-line-down)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "C-f") 'exwm-sm-core-scroll-page-down)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "C-y") 'exwm-sm-core-scroll-line-up)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "RET") 'exwm-sm-core-execute-rep)

;;; Editing text
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "y") 'exwm-sm-core-copy)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "d") 'exwm-sm-core-cut)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "p") 'exwm-sm-core-paste)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "u") 'exwm-sm-core-undo)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "C-r") 'exwm-sm-core-redo)

;;; Learn operations
;; (evil-define-key 'normal exwm-sm-evil-mode-map (kbd "d") 'exwm-sm-core-contents-dismiss-element)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "x") 'exwm-sm-core-extract)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "X") 'exwm-sm-core-schedule-extract)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "SPC") 'exwm-sm-core-learn)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd ",") 'nanjigen/sm-hydra/body)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "P") 'exwm-sm-core-prioritize)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "s") 'exwm-sm-core-reschedule)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "o") 'exwm-sm-core-get-filepath)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "r") 'exwm-sm-core-replay)

(map! :map exwm-sm-evil-mode-map
      :n "SPC" #'exwm-sm-core-learn)

;; Selection
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "V") '(lambda
                                                             ()
                                                             (interactive)
                                                             (exwm-input--fake-key 'end)
                                                             (exwm-sm-evil-visual)
                                                             (exwm-input--fake-key 'up)))
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "v") '(lambda
                                                             ()
                                                             (interactive)
                                                             (exwm-sm-evil-visual)
                                                             (exwm-input--fake-key 'right)))

;;;; Visual
;; Basic movements
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "k") 'exwm-sm-core-up)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "j") 'exwm-sm-core-down)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "h") 'exwm-sm-core-left)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "l") 'exwm-sm-core-right)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "K") 'exwm-sm-core-goto-parent)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "J") 'exwm-sm-core-goto-child)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "H") 'exwm-sm-core-back)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "L") 'exwm-sm-core-forward)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "w") 'exwm-sm-core-word)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "b") 'exwm-sm-core-word-back)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "{") 'exwm-sm-core-visual-backward-paragraph)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "}") 'exwm-sm-core-visual-forward-paragraph)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "b") 'exwm-sm-core-word-back)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "/") 'exwm-sm-core-find)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "t") 'exwm-sm-core-test)
;; (evil-define-key 'normal exwm-sm-evil-mode-map (kbd "<escape>") 'exwm-sm-core-escape)
(evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "<return>") '(lambda () (interactive) (exwm-input--fake-key 'return)))
(evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "RET") '(lambda () (interactive) (exwm-input--fake-key 'return)))

;;; Motion State
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "gg") 'exwm-sm-core-goto-first-line)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "G") 'exwm-sm-core-goto-last-line)

(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "gq") 'exwm-sm-core-edit-question)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "ga") 'exwm-sm-core-edit-answer)

(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "C-u") 'exwm-sm-core-scroll-up)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "C-d") 'exwm-sm-core-scroll-up)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "C-b") 'exwm-sm-core-scroll-page-up)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "C-e") 'exwm-sm-core-scroll-line-down)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "C-f") 'exwm-sm-core-scroll-page-down)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "C-y") 'exwm-sm-core-scroll-line-up)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "RET") 'exwm-sm-core-execute-rep)

;; Selection
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "y") 'exwm-sm-core-copy)

;;; Editing text
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "y") 'exwm-sm-core-copy)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "d") 'exwm-sm-core-cut)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "p") 'exwm-sm-core-paste)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "u") 'exwm-sm-core-undo)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "C-r") 'exwm-sm-core-redo)

;;; Learn operations
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "x") 'exwm-sm-core-extract)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "X") 'exwm-sm-core-schedule-extract)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "SPC") 'exwm-sm-core-learn)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd ",") 'nanjigen/sm-hydra/body)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "P") 'exwm-sm-core-prioritize)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "s") 'exwm-sm-core-reschedule)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "o") 'exwm-sm-core-get-filepath)
(evil-define-key 'visual exwm-sm-evil-mode-map (kbd "r") 'exwm-sm-core-replay)

(map! :map exwm-sm-evil-mode-map
      :n "SPC" #'exwm-sm-core-learn)

;;; Mode
;;;###autoload
(define-minor-mode exwm-sm-evil-mode nil nil nil exwm-sm-evil-mode-map
  (if exwm-sm-evil-mode
      (progn
        (exwm-sm-evil-normal))))

;;;###autoload
(defun exwm-sm-evil-activate-if-sm ()
  "Activates exwm-sm mode when buffer is SM.
SM variant can be assigned in 'exwm-sm-evil-sm-name`"
  (interactive)
  (if (member exwm-class-name exwm-sm-evil-sm-class-name)
      (exwm-sm-evil-mode 1)))

(provide '+exwm-sm-evil)

;;; +exwm-sm-evil.el ends here
