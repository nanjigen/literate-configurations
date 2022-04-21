;;; lang/org-private/+ir.el -*- lexical-binding: t; -*-

(defun nanjigen/org-noter-extract ()
  "Extract highlighted text into org-noter buffer as org-drill item"
  (interactive)
  ;; (org-back-to-heading)
  (save-window-excursion
  ;; Now I need to move the header text (which is the extracted pdf text) into the body
    (progn
      (nanjigen/move-headline-to-contents)
      (org-toggle-tag "extract"))))

(defun nanjigen/open-link-clipboard ()
  (interactive)
  (let ((link (substring-no-properties (x-get-selection 'CLIPBOARD 'STRING))))
    (org-link-open-from-string link)))

;; (org-noter "[[pdf:/home/vrika/Documents/PDF/books/wickedcoolshellscripts.pdf::1]]")
;; (org-brain-open-org-noter "3f0bea0e-07dd-4e48-9df7-aaeb8f1d3b8d")

(defun nanjigen/move-headline-to-contents ()
  "Move extracted PDF text to body of subtree"
  (interactive)
  ;; (let ((origin-element-type (org-element-type (org-element-at-point))))
  ;;   (if (string-equal "property-drawer" origin-element-type)
  ;;       (evil-insert-newline-below)))
  (progn
    ;; (with-selected-window (org-noter--get-notes-window)
    ;;   (evil-insert-newline-below))
    (org-noter-insert-note-toggle-no-questions)
    (org-back-to-heading)
    (org-toggle-tag "extract")
    (let* ((parent
            (save-excursion
              (org-backward-element)
              (org-element-property :title (org-element-at-point))))
           (headline (org-element-at-point))
           (title (org-element-property :title headline))
           (property-end (org-element-property :contents-end headline))
           ;; (property (org-element-at-point (goto-char property-pos)))
           (indent (org-element-property :level headline))
           (title-start (+ indent (org-element-property :begin headline)))
           (title-end (- (org-element-property :contents-begin headline) 1))
           (annot-id (car (last (s-split ";;" (org-entry-get nil "NOTER_PAGE")))))
           (annot-id-clean (s-chop-suffix "]]" annot-id))
           (org-uuid (org-entry-get nil "ID"))
           (annot-link (format "[[brain:%s][%s]]" org-uuid annot-id-clean))
           (cite-page (with-selected-window (org-noter--get-doc-window)
                        (format "%s"
                                (eval '(nth (1- (pdf-view-current-page))
                                            (pdf-cache-pagelabels))))))
           (cite (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      ;; (when (eq 'property-drawer (car property))
      ;; (goto-char (org-element-property :end property)))
      (goto-char property-end)
      (insert title)
      (delete-region title-start title-end)
      (goto-char title-start)
      ;;HACK use logic to identify if parent is an org-noter-outline headline
      ;; and then indent with org-mode code, not a hacky *!
      (insert (format " [[cite:%s][p%s]] %s" cite cite-page annot-link))
      ;; (save-excursion
        ;; (if (not (string-prefix-p "cite" parent))
        ;;     ;; (nanjigen/org-move-to-sibling)
        ;;     (nanjigen/org-move-to-extreme)
        ;;   )
      ;;
      ;;     (insert (format " cite:%s %s" cite id-clean))
      ;;   (insert (format "* cite:%s %s" cite id-clean)))
      (org-back-to-heading))))

(defun nanjigen/org-move-to-extreme ()
  "Move current org subtree to the start of its parent."
  (interactive)
  (condition-case err
      (while t
        (funcall 'org-move-subtree-up)))
    (user-error
     (let ((err-msg (cadr err)))
       (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
         (signal 'user-error (list err-msg))))))

(defun nanjigen/org-move-to-sibling ()
  "Move extracted org subtree under its 'cite:' sibling."
  (interactive)
  (setq parent (nanjigen/org-get-above-headline))
  (while (not (string-prefix-p "cite" parent))
     (funcall 'org-move-subtree-up)
     (setq parent (nanjigen/org-get-above-headline))))

(defun nanjigen/org-get-above-headline ()
"Get the headline entry of above parent/sibling as text"
(save-excursion
  (org-backward-element)
  (org-element-property :title (org-element-at-point))))

(defun nanjigen/chopper ()
  "get 'annot' id from 'NOTER_PAGE' property"
  (interactive)
  (let* ((id (car (last (s-split ";;" (org-entry-get nil "NOTER_PAGE")))))
         (id-clean (s-chop-suffix "]]" id)))
    (insert (format "%s" id-clean))))

(defun nanjigen/delete-headline ()
  "delete the original title of text extracted with
(org-noter-insert-note-toggle-no-questions)"
  (interactive)
  (let* ((headline (org-element-at-point))
         (title-start (org-element-property :contents-begin headline))
         (title-end (org-element-property :end headline)))
    (delete-region title-start title-end)))

;; Functions for capturing from org noter
  (defun nanjigen/get-noter-link ()
    "return PROPERTY value of pdftools link"
    (interactive)
    (let ((linkStr (org-entry-get nil "NOTER_PAGE")))
      (insert linkStr)))

  ;;https://emacs.stackexchange.com/questions/32283/how-to-capture-and-link-code-comments-as-org-agenda-items
  (defun capture-noter-link ()
    "insert PROPERTY value of pdftools link"
    (interactive)
    (let ((linkStr
            (save-excursion
              (save-window-excursion
                (switch-to-buffer (plist-get org-capture-plist :original-buffer))
                (car (org-get-outline-path)))
              )))
      ))


  (defun insert-key (&optional p)
    "insert header as key value of org-brain header"
    ;; (unless p (setq p "NOTER_PAGE"))
    ;; (message "property passed is: %s" p)
    (interactive)
    (let ((pvalue
            (save-window-excursion
              (message "%s" (org-capture-get :original-buffer))
              (switch-to-buffer (org-capture-get :original-buffer))
              (message "retrieved header (key) is: %s" (car (org-get-outline-path (point) p)))
              (car (org-get-outline-path (point) p))
              )))
      pvalue))


  (defun key-to-header (&optional p)
    "insert header in org-capture target file as key value of org-brain header"
    ;; (unless p (setq p "NOTER_PAGE"))
    ;; (message "property passed is: %s" p)
    (interactive)
    (let ((heading
            (save-window-excursion
              (message "%s" (org-capture-get :original-buffer))
              (switch-to-buffer (org-capture-get :original-buffer))
              (message "retrieved header (key) is: %s" (car (org-get-outline-path (point) p)))
              (car (org-get-outline-path (point) p))
              )))
      (goto-char (org-find-exact-headline-in-buffer "IR Cards"))
      (unless (search-forward (format "** %s" heading) nil t)
        (org-end-of-subtree)
        (insert (format "\n** %s" heading))
        )))

  (defun insert-property (&optional p)
    "insert PROPERTY value of pdftools link"
    (interactive)
    (unless p (setq p "NOTER_PAGE"))
    (message "property passed is: %s" p)
    (let ((pvalue
            (save-window-excursion
              (message "%s" (org-capture-get :original-buffer))
              (switch-to-buffer (org-capture-get :original-buffer))
              (message "retrieved property is: %s" (org-entry-get (point) p))
              (org-entry-get (point) p)
              )))
      pvalue))

  (defun follow-noter-page-link ()
    "return PROPERTY value of pdftools link and follow in other-window"
    (interactive)
    ;; (run-with-timer 3 nil (lambda ()
      ;; TODO switch-to-buffer?
    (let ((linkStr (org-entry-get nil "NOTER_PAGE")))
      (if (> (length (window-list)) 1)
          (other-window 1)
        ;; (balance-windows)
          ;; (switch-to-buffer-other-window)
        (split-window-right))
      (org-link-open-from-string linkStr)))
;;
