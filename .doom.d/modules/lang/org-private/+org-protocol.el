;;; lang/org-private/+org-protocol.el -*- lexical-binding: t; -*-

(use-package! org-protocol
  :after org
  :config
  
  (add-to-list 'org-protocol-protocol-alist
               '("org-id" :protocol "org-id"
                 :function org-id-protocol-goto-org-id))
  
  (defun org-id-protocol-goto-org-id (info)
    "This handler simply goes to the org heading with given id using emacsclient.
  
      INFO is an alist containing additional information passed by the protocol URL.
      It should contain the id key, pointing to the path of the org id.
  
        Example protocol string:
        org-protocol://org-id?id=309A0509-81BE-4D51-87F4-D3F61B79EBA4"
    (when-let ((id (plist-get info :id)))
      (org-id-goto id))
    nil)
  
  (defun org-id-protocol-link-copy ()
    (interactive)
    (org-kill-new (concat "org-protocol://org-id?id="
                          (org-id-copy))))
  
  (add-to-list 'org-protocol-protocol-alist
               '("brain-id" :protocol "brain-id"
                 :function brain-id-protocol-visualize-brain-id))
  
  (defun brain-id-protocol-visualize-brain-id (info)
    "This handler visualizes the org heading with given id using emacsclient.
  
      INFO is an alist containing additional information passed by the protocol URL.
      It should contain the id key, pointing to the path of the org id.
  
        Example protocol string:
        org-protocol://brain-id?id=309A0509-81BE-4D51-87F4-D3F61B79EBA4"
    (when-let ((id (plist-get info :id)))
      (org-brain-visualize (or (org-brain-entry-from-id id))))
    nil)
  
  (defun brain-id-protocol-link-copy ()
    (interactive)
    (org-kill-new (concat "org-protocol://brain-id?id="
                          (org-id-copy))))
  
  (add-to-list 'org-protocol-protocol-alist
               '("pdf-tools" :protocol "pdf-tools"
                 :function pdftools-protocol-pop-pdf))
  
  (defun pdftools-protocol-pop-pdf (info)
    "This opens the highlight location of the given extract
  
      INFO is an alist containing additional information passed by the protocol URL.
      It should contain the org-pdf-tools link, pointing to the path of the org id.
  
        Example protocol string:
        org-protocol://brain-id?id=309A0509-81BE-4D51-87F4-D3F61B79EBA4"
    (when-let ((link (plist-get info :pdf)))
      (org-link-open-from-string (format "[[pdf:%s]]" link)))
    nil)
  
  (defun pdf-tools-protocol-link-copy ()
    (interactive)
    (org-kill-new (concat "org-protocol://pdf-tools?pdf="
                          (org-entry-get nil "NOTER_PAGE"))))
  
  (defun pdft-tools-protocol-htmlfier ()
    (interactive)
    (let ((pdf-link (org-entry-get nil "ID")))
      (format "<a href=\"org-protocol://pdf-tools?pdf=%s\"></a>" pdf-link)))
  
  (defun pdf-tools-protocol-html-link-copy ()
    (interactive)
    (org-kill-new (concat "<a href=\"org-protocol://pdf-tools?pdf="
                          (org-entry-get nil "NOTER_PAGE") "</a>")))
  
  (defun org-attach-id-from-dir (id)
    "Translate a org-attach dir folder-path back into an UUID ID"
    (format "%s%s"
    (substring id 0 2)
    (substring id 3)))
  
  (add-to-list 'org-protocol-protocol-alist
               '("media-link" :protocol "media-link"
                 :function media-link-protocol-play-mpv-video))
  
  (defun media-link-protocol-play-mpv-video (info)
    "This handler visualizes the org heading with given id using emacsclient.
  
      INFO is an alist containing additional information passed by the protocol URL.
      It should contain the id key, pointing to the path of the org id.
  
        Example protocol string:
        org-protocol://media-link?video=~/org/.attach/27/e2318b-7353-4004-943a-2f1d69b32209/doge_vid420.mpg#0:00:13"
    (when-let ((link (plist-get info :video)))
      (let* ((org-style-link (format "[[video:%s]]" link))
             (splitted (split-string org-style-link "/"))
             (id (format "%s%s"
                         (nth 3 splitted)
                         (nth 4 splitted))))
          ;; (org-link-open-from-string (format "[[video:%s]]" link))
          (setq org-protocol-uuid-from-media-link id)
          (setq org-protocol-last-visited-link org-style-link)
          (org-link-open-from-string org-style-link))
          nil))
  
    (defun nanjigen/org-media-note-jump-to-note ()
      (interactive)
      (let* ((buffer (org-id-find org-protocol-uuid-from-media-link))
            (link-text (nth 5
                            (split-string org-protocol-last-visited-link "/")))
            (link-text-clean (substring link-text 0 -2)))
        (progn
          (org-id-open org-protocol-uuid-from-media-link t)
          (org-narrow-to-subtree)
          (search-forward link-text-clean)
          (recenter nil))))
  
  (defun media-link-protocol-link-copy ()
    (interactive)
    (org-kill-new (concat "org-protocol://media-link?video="
                          (org-id-copy))))
  
  (defun nanjigen/org-media-note--create-session ()
    (let* ((notes-buffer
            (make-indirect-buffer)))))
  (defun media-link-protocol-play-cite-video (info)
  "This handler visualizes the org heading with given id using emacsclient.
  
    INFO is an alist containing additional information passed by the protocol URL.
    It should contain the id key, pointing to the path of the org id.
  
      Example protocol string:
      org-protocol://media-link?videocite=~/org/.attach/27/e2318b-7353-4004-943a-2f1d69b32209/doge_vid420.mpg#0:00:13"
  (when-let ((link (plist-get info :videocite)))
    (org-link-open-from-string (format "[[videocite:%s]]" link)))
  nil)
  
  (defun media-link-protocol-cite-link-copy ()
    (interactive)
    (org-kill-new (concat "org-protocol://media-link?videocite="
                          (org-media-note--current-org-ref-key))))
  )

(cl-defgeneric brain-link-export (path desc backend)
 "Generic function to export a brain link."
 path)

;; this one runs when the backend is equal to html
(cl-defmethod brain-link-export ((path t) (desc t) (backend (eql html)))
 (format "<a href=\"org-protocol://brain-id?id=%s\">%s</a>" path desc))

(org-link-set-parameters "brain" :export 'brain-link-export)
(cl-defgeneric media-link-export (path desc backend)
 "Generic function to export a brain link."
 path)

;; this one runs when the backend is equal to html
(cl-defmethod media-link-export ((path t) (desc t) (backend (eql html)))
 (format "<a href=\"org-protocol://media-link?video=%s\">%s</a>" path desc))

(org-link-set-parameters "video" :export 'media-link-export)

;; this one runs when the backend is equal to html
(cl-defmethod media-link-export ((path t) (desc t) (backend (eql html)))
 (format "<a href=\"org-protocol://media-link?video=%s\">%s</a>" path desc))

(org-link-set-parameters "video" :export 'media-link-export)

;; for videocite links
(cl-defgeneric videocite-link-export (path desc backend)
 "Generic function to export a brain link."
 path)

;; this one runs when the backend is equal to html
(cl-defmethod videocite-link-export ((path t) (desc t) (backend (eql html)))
 (format "<a href=\"org-protocol://media-link?videocite=%s\">%s</a>" path desc))

(org-link-set-parameters "videocite" :export 'videocite-link-export)
(cl-defgeneric cite-link-export (path desc backend)
 "Generic function to export a brain link."
 path)

;; this one runs when the backend is equal to html
(cl-defmethod cite-link-export ((path t) (desc t) (backend (eql html)))
  (format "<a href=\"%s\">%s:%s</a>" path path desc))

(org-link-set-parameters "cite" :export 'cite-link-export)
(after! org
  (setq org-html-self-link-headlines t)

  (defun org-html-headline (headline contents info)
    "Transcode a HEADLINE element from Org to HTML.
    CONTENTS holds the contents of the headline.  INFO is a plist
    holding contextual information."
    (unless (org-element-property :footnote-section-p headline)
      (let* ((numberedp (org-export-numbered-headline-p headline info))
             (numbers (org-export-get-headline-number headline info))
             (level (+ (org-export-get-relative-level headline info)
                       (1- (plist-get info :html-toplevel-hlevel))))
             (todo (and (plist-get info :with-todo-keywords)
                        (let ((todo (org-element-property :todo-keyword headline)))
                          (and todo (org-export-data todo info)))))
             (todo-type (and todo (org-element-property :todo-type headline)))
             (priority (and (plist-get info :with-priority)
                            (org-element-property :priority headline)))
             (text (org-export-data (org-element-property :title headline) info))
             (tags (and (plist-get info :with-tags)
                        (org-export-get-tags headline info)))
             (full-text (funcall (plist-get info :html-format-headline-function)
                                 todo todo-type priority text tags info))
             (contents (or contents ""))
             (id (org-html--reference headline info))
             (brain-id (org-element-property :ID headline))
             (formatted-text
              ;; (if (plist-get info :html-self-link-headlines)
              ;;     (format "<a href=\"#%s\">%s</a>" id full-text)
              ;;   full-text)))
              (if (plist-get info :html-self-link-headlines)
                  (format "<a href=\"org-protocol://brain-id?id=%s\">%s</a>" brain-id full-text)
                full-text)))
        (if (org-export-low-level-p headline info)
            ;; This is a deep sub-tree: export it as a list item.
            (let* ((html-type (if numberedp "ol" "ul")))
              (concat
               (and (org-export-first-sibling-p headline info)
                    (apply #'format "<%s class=\"org-%s\">\n"
                           (make-list 2 html-type)))
               (org-html-format-list-item
                contents (if numberedp 'ordered 'unordered)
                nil info nil
                (concat (org-html--anchor id nil nil info) formatted-text)) "\n"
               (and (org-export-last-sibling-p headline info)
                    (format "</%s>\n" html-type))))
          ;; Standard headline.  Export it as a section.
          (let ((extra-class
                 (org-element-property :HTML_CONTAINER_CLASS headline))
                (headline-class
                 (org-element-property :HTML_HEADLINE_CLASS headline))
                (first-content (car (org-element-contents headline))))
            (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                    (org-html--container headline info)
                    (format "outline-container-%s" id)
                    (concat (format "outline-%d" level)
                            (and extra-class " ")
                            extra-class)
                    (format "\n<h%d id=\"%s\"%s>%s</h%d>\n"
                            level
                            id
                            (if (not headline-class) ""
                              (format " class=\"%s\"" headline-class))
                            (concat
                             (and numberedp
                                  (format
                                   "<span class=\"section-number-%d\">%s</span> "
                                   level
                                   (mapconcat #'number-to-string numbers ".")))
                             formatted-text)
                            level)
                    ;; When there is no section, pretend there is an
                    ;; empty one to get the correct <div
                    ;; class="outline-...> which is needed by
                    ;; `org-info.js'.
                    (if (eq (org-element-type first-content) 'section) contents
                      (concat (org-html-section first-content "" info) contents))
                    (org-html--container headline info)))))))
  )
