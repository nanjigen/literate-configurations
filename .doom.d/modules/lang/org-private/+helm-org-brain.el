;;; lang/org-private/+helm-org-brain.el -*- lexical-binding: t; -*-

(defmacro helm-exit-and-run! (&rest body)
  "Define an action with BODY to be run after exiting Helm."
  (declare (doc-string 1))
  `(lambda ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action (lambda (_candidate) ,@body)))))

  (defun helm-brain--add-children (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-add-relationship
       (org-brain-entry-at-pt) (or (org-brain-entry-from-id candidate) candidate)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--add-parents (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-add-relationship
       (or (org-brain-entry-from-id candidate) candidate) (org-brain-entry-at-pt)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--add-friends (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain--internal-add-friendship
       (org-brain-entry-at-pt) (or (org-brain-entry-from-id candidate) candidate)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--delete-entries (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-delete-entry (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-brain--archive (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-archive (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-brain--select (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-select (or (org-brain-entry-from-id candidate) candidate) 1)))

  (defun helm-brain--unselect (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-select (or (org-brain-entry-from-id candidate) candidate) -1)))

(defun helm-org-brain--visualize-node (_c)
  (dolist (candidate (helm-marked-candidates))
    (org-brain-visualize (or (org-brain-entry-from-id candidate) candidate))))

(defun helm-org-brain--switch-node (_c)
  (dolist (candidate (helm-marked-candidates))
    (org-brain-goto (or (org-brain-entry-from-id candidate) candidate))))

(defun helm-org-brain-switch-node-other-window ()
  "Open the current node selected in helm-brain in org"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-brain--switch-node)))

(defvar helm-org-brain-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; (define-key map (kbd "C-c o") (helm-exit-and-run! (helm-org-brain--switch-node)))
    (define-key map (kbd "C-c o") 'helm-org-brain-switch-node-other-window)
    map)
  "Keymap for `helm-brain'.")

(defun helm-org-brain-build-source (&optional filter)
  "Build source for org-brain buffers.
See `helm-org-brain' for more details."
  (helm-build-sync-source "org-brain"
    :candidates #'org-brain--all-targets
    ;; :candidate-transformer 'helm-exwm-highlight-buffers
    :action '(("Visualize" . (lambda (x)
                   (org-brain-visualize (or (org-brain-entry-from-id x) x))))
              ;; ("Visualize node" . helm-org-brain--visualize-node)
              ("Switch to node(s) in other window `C-c o`'" . helm-org-brain--switch-node)
              ("Kill buffer(s) `M-D`" . helm-kill-marked-buffers))
    ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
    :persistent-action 'helm-buffers-list-persistent-action
    :keymap helm-org-brain-map))

  (defvar helm-org-brain--fallback-source
    (helm-make-source "New entry" 'helm-source-dummy
      :action (helm-make-actions
               "Visualize" (lambda (x)
                             (org-brain-visualize (org-brain-get-entry-from-title x)))
               "Add children" 'helm-brain--add-children
               "Add parents" 'helm-brain--add-parents
               "Add friends" 'helm-brain--add-friends)))

;; (defun helm-org-brain (&optional filter)
(defun helm-org-brain ()
  (interactive)
  (helm :sources (helm-org-brain-build-source helm-brain--fallback-source)
        :buffer "helm-org-brain"))
