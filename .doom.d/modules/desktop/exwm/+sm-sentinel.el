;;; desktop/exwm/+sm-sentinel.el -*- lexical-binding: t; -*-

(defun sm-element-inotify-process ()
  "Watch for reads of element files in the collection dir"
  (interactive)
  (start-process
   "inotifywait"
   "*inotifywait*"
   "inotifywait" "-mrq" "-e" "open" "/home/nanjigen/Documents/SM/neuron/elements" "--include" ".HTM"))

(defun sm-image-inotify-process ()
  "Watch for reads of image files in the collection dir"
  (interactive)
  (start-process
   "inotifywait-image"
   "*inotifywait-image*"
   "inotifywait" "-mrq" "-e" "open" "/home/nanjigen/Documents/SM/neuron/elements" "--include" "\.(jpg|gif|png|svg)"))

(defun msg-me (process event)
  (princ
   (format "Process: %s had the event '%s'" process event)))
(set-process-sentinel (get-process "inotifywait") 'msg-me)

(defun keep-output (process output)
  "Store the paths of elements caught by inotify"
  (progn
    (setq captured-path (cons output captured-path))
    (sleep-for 0.1)
    (list-sm-element-paths)
    (setq captured-path nil)))

(defun keep-image-path (process output)
  "Store the paths of elements caught by inotify"
  (progn
    (setq captured-image-path (cons output captured-image-path))
    (sleep-for 0.1)
    (setq image-file-list (s-lines (s-replace " OPEN " "" (car captured-image-path))))
    (setq captured-image-path nil)))

(defvar captured-image-path nil
  "The path captured by the inotify sentinel")

(defvar captured-path nil
  "The path captured by the inotify sentinel")

(defun list-sm-element-paths ()
  "Text munge captured paths"
  (setq sm-file-list (s-lines (s-replace " OPEN " "" (car captured-path))))
  (interactive)
  (if (> (length sm-file-list) 1)
      (progn
        (setq sm-element-item-p t)
        (setq sm-element-article-p nil))
    (progn
      (setq sm-element-item-p nil)
      (setq sm-element-article-p t))))

(defvar sm-element-item-p nil
  "Whether currently viewed element is an item.")

(defvar sm-element-article-p nil
  "Whether currently viewed element is an article")
