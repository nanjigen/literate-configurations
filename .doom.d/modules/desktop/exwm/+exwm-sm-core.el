;;; desktop/exwm/+exwm-sm-evil.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'exwm-input)

(defun exwm-sm-xdotool-send-key (mod-key keypress)
  "Send a reload event to Firefox."
  (interactive)
  (let ((key keypress)
        ;; (window-id exwm--id)
        (modifier mod-key))
    (shell-command
     (format "xdotool keydown %s sleep 0.1" modifier))
    (exwm-input--fake-key key)
    (shell-command
     (format "xdotool keyup %s"  modifier))))

(defun exwm-input--on-ButtonPress-line-mode (buffer button-event)
  "Handle button events in line mode.
BUFFER is the `exwm-mode' buffer the event was generated
on. BUTTON-EVENT is the X event converted into an Emacs event.

The return value is used as event_mode to release the original
button event."
  (with-current-buffer buffer
    (let ((read-event (exwm-input--mimic-read-event button-event)))
      (exwm--log "%s" read-event)
      (if (and read-event
               (exwm-input--event-passthrough-p read-event))
          ;; The event should be forwarded to emacs
          (progn
            (exwm-input--cache-event read-event)
            (exwm-input--unread-event button-event)

            xcb:Allow:ReplayPointer)
        ;; The event should be replayed
        xcb:Allow:ReplayPointer))))

;;; Basic navigation
;;;###autoload
(defun exwm-sm-core-up ()
  "Move up."
  (interactive)
  (exwm-input--fake-key 'up))

;;;###autoload
(defun exwm-sm-core-down ()
  "Move down."
  (interactive)
  (exwm-input--fake-key 'down))

;;;###autoload
(defun exwm-sm-core-left ()
  "Move down."
  (interactive)
  (exwm-input--fake-key 'left))

;;;###autoload
(defun exwm-sm-core-right ()
  "Move down."
  (interactive)
  (exwm-input--fake-key 'right))

;;;###autoload
(defun exwm-sm-core-word ()
  "Word noun"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'right))

;;;###autoload
(defun exwm-sm-core-word-back ()
  "Back word"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'left))

;;;###autoload
(defun exwm-sm-core-beginning-of-line ()
  "Go to line start"
  (interactive)
  (exwm-input--fake-key 'home))

;;;###autoload
(defun exwm-sm-core-end-of-line ()
  "Go to line end"
  (interactive)
  (exwm-input--fake-key 'end))

;;;###autoload
(defun exwm-sm-core-goto-parent ()
  "Go to parent of current element"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'up))

;;;###autoload
(defun exwm-sm-core-goto-child ()
  "Go to the first child of current element"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'down))

;;;###autoload
(defun exwm-sm-core-forward ()
  "Go forward element"
  (interactive)
  (exwm-sm-xdotool-send-key "alt" 'right))

;;;###autoload
(defun exwm-sm-core-back ()
  "Go back element"
  (interactive)
  (exwm-sm-xdotool-send-key "alt" 'left))

;;;###autoload
(defun exwm-sm-core-goto-first-line ()
  "Go to top of component page"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'home))

;;;###autoload
(defun exwm-sm-core-goto-last-line ()
  "Go to bottom of component page
Note that this goes past the references"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'end))

;;;###autoload
(defun exwm-sm-core-scroll-up ()
  "Scroll up"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl+End"))

;;;###autoload
(defun exwm-sm-core-scroll-down ()
  "Scroll Down"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl+End"))

;;;###autoload
(defun exwm-sm-core-scroll-page-up ()
  "Scroll up by page length"
  (interactive)
  (exwm-sm-xdotool-send-key "Page_Up"))

;;;###autoload
(defun exwm-sm-core-scroll-line-down ()
  "Scroll one visual line down"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl+End"))

;;;###autoload
(defun exwm-sm-core-scroll-page-down ()
  "Scroll down by page length"
  (interactive)
  (exwm-sm-xdotool-send-key "Page_Down"))

;;;###autoload
(defun exwm-sm-core-scroll-line-up ()
  "Scroll one visual line up"
  (interactive)
  (exwm-sm-xdotool-send-key "Page_Down"))

;;;; Selection

;;;###autoload
(defun exwm-sm-core-visual-char ()
  "Start visual char selection."
  (interactive)
  (exwm-sm-xdotool-send-key "Shift" 'right))

;;;###autoload
(defun exwm-sm-core-visual-line ()
"Start visual line selection."
  (interactive)
  (exwm-input--fake-key 'home)
  (exwm-sm-xdotool-send-key "Shift+End")
  (evil-visual-state))

;;;###autoload
(defun exwm-sm-core-select-all ()
  "Select whole page."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl+a"))

;;; Find/Search

;;;###autoload
(defun exwm-sm-core-find ()
  "Find general."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f))

;;;###autoload
(defun exwm-sm-core-search ()
  "Search for texts containing a given string."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'r))

;;;###autoload
(defun exwm-sm-core-find-elements ()
  "Find elements."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f))

;;;###autoload
(defun exwm-sm-core-search-phrase ()
  "Search currently selected phrase."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f3))

;;;###autoload
(defun exwm-sm-core-search-element-id ()
  "Goto element with a given element-id"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'g))

;;;###autoload
(defun exwm-sm-core-find-next ()
  "Find next."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'g))

;;;###autoload
(defun exwm-sm-core-find-previous ()
  "Find previous."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl Shift" 'g))

;;; Editing

;;;###autoload
(defun exwm-sm-core-escape ()
  (interactive)
  (exwm-input--fake-key 'escape))

;;;###autoload
(defun exwm-sm-core-paste ()
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'v))

(defun exwm-sm-core-paste-html ()
  "Paste html from clipboard."
   (interactive)
   (exwm-sm-xdotool-send-key "Shift" 'f10)
   (exwm-input--fake-key 'x)
   (exwm-input--fake-key 'p))

;;;###autoload
(defun exwm-sm-core-copy ()
  "Copy to clipboard."
   (interactive)
   (exwm-sm-xdotool-send-key "ctrl" 'c))

;;;###autoload
(defun exwm-sm-core-cut ()
  "Cut text."
   (interactive)
   (exwm-sm-xdotool-send-key "ctrl" 'x))

;;;###autoload
(defun exwm-sm-core-undo ()
  "Undo."
   (interactive)
   (exwm-sm-xdotool-send-key "ctrl" 'u))

;;;###autoload
(defun exwm-sm-core-redo ()
  "Redo."
   (interactive)
   (exwm-sm-xdotool-send-key "ctrl Shift" 'z))

;;;###autoload
(defun exwm-sm-core-bold ()
  "Embolden selected text"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'b))

;;;###autoload
(defun exwm-sm-core-italic ()
  "Italicise selected text."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'i))

;;;###autoload
(defun exwm-sm-decrease-font ()
  "Decrease selected font size"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" '\[))

;;;###autoload
(defun exwm-sm-increase-font ()
  "Increase selected font size"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" '\]))

;;;###autoload
(defun exwm-sm-core-edit-question ()
  "Edit the question component"
  (interactive)
  (exwm-input--fake-key 'escape)
  (exwm-input--fake-key 'q))

;;;###autoload
(defun exwm-sm-core-edit-answer ()
  "Edit the first answer"
  (interactive)
  (exwm-input--fake-key 'escape)
  (exwm-input--fake-key 'a))

;;;###autoload
(defun exwm-sm-core-edit-file ()
  "Edit .HTM file"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f9))

;;;###autoload
(defun exwm-sm-core-edit-next-component ()
  "Edit the next component in element window"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 't))

;;;###autoload
(defun exwm-sm-core-edit-switch-mode ()
  "Cycle presentation -> editing -> dragging modes"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'e))

;;;###autoload
(defun exwm-sm-core-elements-dismiss-element ()
"Dismiss element while in element window"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'd))

;;;###autoload
(defun exwm-sm-core-element-params ()
  "Bring up apply template menu"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'p))

;;;###autoload
(defun exwm-sm-core-insert-image ()
  "Insert image into component."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'F8))

;;;###autoload
(defun exwm-sm-core-insert-splitline ()
  "Insert splitline in the component menu"
  (interactive)
  (exwm-sm-xdotool-send-key "Shift alt" 'h))

;;;###autoload
(defun exwm-sm-core-cycle-image-stretch ()
  "Cycle stretch of image component."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'q))

;;;###autoload
(defun exwm-sm-core-apply-template ()
  "Bring up apply template menu"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl Shift" 'm))

;;;###autoload
(defun exwm-sm-core-item-picture ()
  "Apply Item Picture template to element"
  (interactive)
  (exwm-sm-core-apply-template)
  (exwm-sm-xdotool-send-string "Item Article"))

;;;###autoload
(defun exwm-sm-core-item-picture ()
  "Apply Item Picture template to element"
  (interactive)
  (exwm-sm-core-apply-template)
  (exwm-sm-xdotool-send-string "Item Picture"))

;;;###autoload
(defun exwm-sm-core-import-component ()
  "Import component in element window"
  (interactive)
  ;; TODO direct this to the `sm-element-window'
  (exwm-sm-xdotool-send-key "ctrl" 'q))

;;;###autoload
(defun exwm-sm-core-reorder-components ()
  "Bring up reorder components menu"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'o))

;;;###autoload
(defun exwm-sm-core-reference-label ()
  "Bring up references menu"
  (interactive)
  (exwm-sm-xdotool-send-key "alt" 'q))

;;;###autoload
(defun exwm-sm-core-test-rep-cycle ()
  "Emulate learning mode to test element/components"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl alt" 'l))

;;;###autoload
(defun exwm-sm-core-tile-components ()
  "Go into tiling menu for component tiling"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl alt" 't))

(defun exwm-sm-core-ancestors ()
"Bring up element ancestors menu"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl Shift" 'x))

(defun exwm-sm-core-create-hyperlink ()
"Create hyperlink over selected text"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'k))

;;; Learn operations

;;;###autoload
(defun exwm-sm-core-learn ()
  "Start learning."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'l))

;;;###autoload
(defun exwm-sm-core-execute-rep ()
  "Execute repition."
  (interactive)
  (exwm-input--fake-key 'enter))

;;;###autoload
(defun exwm-sm-core-replay ()
  "Replay component."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'F10))

;;;###autoload
(defun exwm-sm-core-cloze ()
  "Extract selected text."
  (interactive)
  (exwm-sm-xdotool-send-key "alt" 'z))

;;;###autoload
(defun exwm-sm-core-extract ()
  "Extract selected text."
  (interactive)
  (exwm-sm-xdotool-send-key "alt" 'x))

;;;###autoload
(defun exwm-sm-core-schedule-extract ()
  "Extract and schedule for a later time."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl alt" 'x))

;;;###autoload
(defun exwm-sm-core-prioritize ()
  "Modify priority of current element."
  (interactive)
  ;;TODO perhaps enter char-mode or create hydra on window
  (exwm-sm-xdotool-send-key "alt" 'p))

;;;###autoload
(defun exwm-sm-core-reschedule ()
  "Learning: Reschedule to another day."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'j))

;;;###autoload
(defun exwm-sm-core-postpone ()
  "Schedule review later today."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl Shift" 'j))

;;;###autoload
(defun exwm-sm-core-remember ()
  "Introduce element into learning que."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'm))

;;;###autoload
(defun exwm-sm-core-execute-rep ()
  "Execute a mid-interval rep at later date."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl Shift" 'r))

;;;###autoload
(defun exwm-sm-core-cancel-grade ()
  "Undo grading on element."
  (interactive)
  (exwm-sm-xdotool-send-key "alt" 'g))

;;;###autoload
(defun exwm-sm-core-branch-learning ()
  "Subset review of a selection of a branch in contents."
  (interactive)
;;; TODO somekind of buffer check
  (exwm-sm-xdotool-send-key "ctrl" 'l))

;;;###autoload
(defun exwm-sm-core-random-learning ()
  "Random learning."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f11))

;;;###autoload
(defun exwm-sm-core-random-test ()
  "Random test."
  (interactive)
  ;; TODO target browser window
  (exwm-sm-xdotool-send-key "ctrl" 'f11))

;;;###autoload
(defun exwm-sm-core-set-read-point ()
  "Set the read-point."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f7))

;;;###autoload
(defun exwm-sm-core-mercy ()
  "Activate mercy."
  (interactive)
  (exwm-sm-xdotool-send-key "Shift alt" 'm))

;;;###autoload
(defun exwm-sm-core-neural ()
  "Go neural."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'F2))

;;;###autoload
(defun exwm-sm-core-sorting-criteria ()
  "Open sorting criteria window."
  (interactive)
  (exwm-sm-xdotool-send-key "alt" 'l)
  (exwm-input--fake-key 'o)
  (exwm-input--fake-key 'c))

;;;###autoload
(defun exwm-sm-core-subset-learning ()
  "Subset learning."
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'l))

;;;###autoload
(defun exwm-sm-convert-to-concept ()
  "Convert element to concept"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl+k"))

;;;###autoload
(defun exwm-sm-new-article ()
  "Create new article"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl+n"))

;;; Misc

;;;###autoload
(defun exwm-sm-core-sm-commander ()
  "Summon the SuperMemo Commander"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'return))

;;;###autoload
(defun exwm-sm-core-repair-collection ()
  "Bring up the collection repair menu"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f12))

;;;###autoload
(defun exwm-sm-core-restore-layout ()
  "Restore the default window layout"
  (interactive)
  (exwm-sm-xdotool-send-key "ctrl" 'f5))

;;;###autoload
(defun exwm-sm-core-statistics-analysis ()
  "Open stats window"
  (interactive)
  (exwm-sm-xdotool-send-key "Shift+alt+a"))

;;;###autoload
(defun exwm-sm-core-open-file ()
  "Open file."
  (interactive)
  (exwm-input--fake-key ?\C-o))

;;;###autoload
(defun exwm-sm-core-quit ()
  "Quit sm."
  (interactive)
  (exwm-input--fake-key ?\C-q))

(provide '+exwm-sm-core)
