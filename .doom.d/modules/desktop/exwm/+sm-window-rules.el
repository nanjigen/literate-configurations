;;; desktop/exwm/+sm-window-rules.el -*- lexical-binding: t; -*-

(add-hook 'exwm-update-class-hook
          (defun rename-sm-class ()
            "Rename sm18.exe class"
            (when (string-match (rx "sm18.exe") exwm-instance-name)
              (setq exwm-class-name exwm-instance-name))))

(add-hook 'exwm-update-class-hook
          (defun my-exwm-update-class-hook ()
            (unless (or (string-prefix-p "sm18.exe" exwm-class-name))
              (exwm/rename-buffer))))


(setq exwm-sm-core-buffer-alist
      (list `("sm-element-window" . ,(rx bol "home" eol))
            `("sm-knowledge-tree" . ,(rx bol "neuron" eol))
            '("sm-template-registry" . "Template Registry")
            `("sm-element-data" . ,
               (rx
                (|
                 (seq (or "Topic" "Element") space "#" (one-or-more digit) ":")
                 (seq "Element data"))))
            `("sm-frame" . ,(regexp-quote "z:\\home\\nanjigen\\documents\\sm"))))

(add-hook 'exwm-update-title-hook
          (defun sm-core-window-title-hook ()
            "Manage the core names"
            (cl-loop for (key . value) in exwm-sm-core-buffer-alist
                     if (string-match value exwm-title)
                     do (exwm-workspace-rename-buffer key)
                        (setq exwm-title key))))

(setq exwm-sm-floating-buffer-alist
      '(("sm-images" . "Images")
        ("sm-image-reg" . "Image registry")
        ("sm-sorting-criteria" . "Repetition sorting criteria")
        ("sm-concepts" . "Concepts")
        ("sm-new-concept" . "New Concept")
        ("sm-subset" . "Subset")
        ("sm-repair-options" . "Repair Options")
        ("sm-statistics" . "Analysis")
        ("sm-modify-priority" . "Element Priority")
        ("sm-backup-copy" . "Copying")
        ("sm-info-window" . "Information")
        ("sm-leech-window" . "Leech")
        ("sm-algo-choice" . "Choices")
        ("sm-options-window" . "Options")
        ("sm-error-window" . "Error!")
        ("sm-fonts-window" . "Font")
        ("sm-mercy-parameters" . "^Mercy")
        ("sm-shift-input" . "Input number")
        ("sm-interval" . "Interval")
        ("sm-open-link" . "Open")
        ("sm-question-window" . "Question")
        ("sm-workload-calendar" . "Workload")
        ("sm-outstanding" . "Outstanding")
        ("sm-commander" . "SuperMemo Commander")
        ;; ("sm-template-registry" . "Template Registry")
        ("sm-element-finder" . "Find elements")))

(add-hook 'exwm-update-title-hook
          (defun sm-window-title-hook ()
            "Iterate over naming alist and rename `exwm-title' to key"
            (cl-loop for (key . value) in exwm-sm-floating-buffer-alist
                     if (string-match (regexp-quote value) exwm-title)
                     do (exwm-workspace-rename-buffer key)
                        (setq exwm-title key))))


;; (setq exwm-manage-configurations nil)
;; (add-to-list 'exwm-manage-configurations '((string= exwm-instance-name "sm18-core") managed t floating nil))
(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "sm18.exe") managed t floating nil))

(cl-loop for (key . value) in exwm-sm-core-buffer-alist
         do (push `((string= exwm-title ,key) managed t floating nil) exwm-manage-configurations))

(cl-loop for (key . value) in exwm-sm-floating-buffer-alist
         do (push `((string= exwm-title ,key) managed t floating t) exwm-manage-configurations))

(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "notepad.exe") managed t floating nil))



(add-hook 'exwm-update-title-hook
          (defun my-exwm-update-title-hook ()
            (unless (or (string-prefix-p "sm18.exe" exwm-class-name))
              (exwm/rename-buffer))))


;; (add-to-list 'display-buffer-alist
;;              `((,<<empty-sm>>
;;                 (display-buffer-no-window))
;;                (,<<tree-rx>>
;;                 (display-buffer-reuse-window display-buffer-in-side-window)
;;                 (side . left)
;;                 (slot . 0)
;;                 (window-width . 0.22))
;;                (,<<element-data-rx>>
;;                 (display-buffer-at-bottom)
;;                 ;; (side . bottom)
;;                 ;; (slot . 0)
;;                 (window-height . 0.19))))
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (string-match-p "sm-frame" (buffer-name))
              (display-buffer
               (get-buffer-create "sm-frame")
               '((display-buffer-no-window)
                 (allow-no-window . t))))
            (when (string-match-p "sm-element-window" (buffer-name))
              (display-buffer
               (get-buffer-create "sm-element-window")
               (doom-modeline-mode 1)
               '((display-buffer-same-window))))
            (when (string-match-p "sm-knowledge-tree" (buffer-name))
              (display-buffer
               (get-buffer-create "sm-knowledge-tree")
               '((display-buffer-in-side-window)
                 ;; (inhibit-same-window . t)
                 (side . left)
                 (slot . 0)
                 (window-width . 0.21))))
            (when (string-match-p "sm-element-data" (buffer-name))
              (progn (exwm-layout-hide-mode-line)
                     (display-buffer
                      (get-buffer-create "sm-element-data")
                      '((display-buffer-in-side-window)
                        ;; (inhibit-same-window . t)
                        (side . bottom)
                        (slot . 1)
                        (window-height . 0.24)))))))
;; (remove-hook 'exwm-manage-finish-hook
;;           (lambda ()
;;             (when (string-match-p
;;                    (rx
;;                     (|
;;                      (seq (or "Topic" "Element") space "#" (one-or-more digit) ":")
;;                      (seq "Element data")))
;;                    (buffer-name))
;;               (display-buffer-in-side-window (current-buffer)
;;                                              '((side . bottom)
;;                                                (slot . 0)
;;                                                (window-width . 0.23))))))
;; 

;; 

;; 

;; )
