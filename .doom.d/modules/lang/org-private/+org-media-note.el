;;; lang/org-private/+org-media-note.el -*- lexical-binding: t; -*-

(use-package! org-media-note
  :hook (org-mode . org-media-note-setup-org-ref)
  ;; :bind (("H-n" . org-media-note-hydra/body))  ;; Main entrance
  :config
  (setq bibtex-completion-pdf-field "file") ;; this is needed for local files stored in the bibkey entry, otherwise the path won't be found
  (setq org-media-note-screenshot-save-method 'attach)
  (setq org-media-note-use-refcite-first t)  ;; use videocite link instead of video link if possible

  

  (defun nanjigen/org-media-note-insert-gif ()
    "Use giffer to generate a gif from mpv's loop
  giffer's path is input-file output-file start-time end-time"
    (interactive)
    (let* ((input (shell-quote-argument (mpv-get-property "path")))
           (base-path (url-basepath (mpv-get-property "path")))
           (time-a (mpv-get-property "ab-loop-a"))
           (time-b (mpv-get-property "ab-loop-b"))
           (gif-file-name (org-media-note--format-file-name
                           (format "%sextract-%s-%s.gif" base-path time-a time-b)))
                          ;; (concat base-path "extract-"
                          ;;         time-a time-b ".gif")))
           (gif-target-path (cond
                               ((eq org-media-note-screenshot-save-method
                                    'attach)
                                (expand-file-name gif-file-name
                                                  (org-attach-dir t)))
                               ((eq org-media-note-screenshot-save-method
                                    'directory)
                                (expand-file-name gif-file-name org-media-note-screenshot-image-dir)))))
      (shell-command (format "giffer %s %s %s %s" input gif-file-name time-a time-b))
      (insert (format "[[file:%s]] " gif-file-name)
      ;; (if (and (eq org-media-note-screenshot-save-method
      ;;              'attach)
      ;;          (eq org-media-note-screenshot-link-type-when-save-in-attach-dir
      ;;              'attach))
      ;;     (insert (format "[[attachment:%s]] "
      ;;                     (file-relative-name gif-target-path
      ;;                                         (org-attach-dir))))
      ;;   (insert (format "[[file:%s]] "
      ;;                   (org-media-note--format-file-path gif-target-path)
      ;;                   )))
      (org-media-note--display-inline-images))))

  (defcustom org-media-note-crop-p nil
    "When nil, allow cropping to start, otherwise stop cropping"
    :type 'boolean)
  
  (defun org-media-note-toggle-crop ()
    "toggle cropping"
    (interactive)
    (if org-media-note-crop-p
        (progn
          (mpv-run-command "set" "fullscreen" "no")
          (mpv-run-command "vf" "del" "-1")
          (setq org-media-note-crop-p nil))
      (progn
        (mpv-run-command "set" "fullscreen" "yes")
        (mpv-run-command "script-message-to" "crop" "start-crop")
        (setq org-media-note-crop-p t))))

  (setq org-html-self-link-headlines t)
  
  (pretty-hydra-define+ org-media-note-hydra ()
    ("File"
     (("f" org-media-note-toggle-mirror-view "Toggle mirror flip"))
     "Playback"
     (("," (mpv-seek-backward 1) "Back 1s")
      ("." (mpv-seek-forward 0.5) "Forward 1s"))
     "Toggle"
     (("t c" org-media-note-toggle-crop "Toggle mpv cropping"))))

  )
