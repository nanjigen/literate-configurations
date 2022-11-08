;;; lang/org-private/config.el -*- lexical-binding: t; -*-

;; org mode
(after! org
  :init
  (setq org-archive-location (concat org-directory "archive/archive.org::* From %s"))

  :config
  (setq org-superstar-headline-bullets-list '("› "))
  (setq org-fancy-priorities-mode nil) ;; needed for understanding fine-grained priority
  (setq org-startup-indented t
        org-clock-idle-time 5
        ;; org-bullets-bullet-list '("› ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0
        org-tags-column -79
        org-modules '(org-habit
                      org-eshell
                      org-bookmark))
 ;; org-bibtex
 ;; org-docview
 ;; org-info
 (setq org-image-actual-width (/ (display-pixel-width) 3))

  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (prettify-symbols-mode)))

  (add-hook! 'org-mode-hook #'doom-disable-line-numbers-h)

  (defface org-checkbox-done-text
    '((t (:foreground "#71696A")))
    "Face for the text part of a checked org-mode checkbox.")
  (add-to-list 'org-modules 'org-checklist)

  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-preceding-days 14
        ;; org-habit-following-days 1
        org-habit-graph-column 80
        org-habit-show-habits-only-for-today t
        ;; org-habit-show-all-today t)
        )

 (setq org-sticky-header-full-path 't)

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "#ffb86c" :weight bold)
                ("NEXT" :foreground "#8be9fd" :weight bold)
                ("DONE" :foreground "#6272a4" :weight bold)
                ("IN-PROGRESS" :foreground "#50fa7b" :weight bold)
                ("PROJECT" :foreground "#0189cc" :weight bold)
                ("WAITING" :foreground "#f8f8f2" :weight bold)
                ("HOLD" :foreground "#a0522d" :weight bold)
                ("CANCELLED" :foreground "#ff5555" :weight bold)))))

(use-package! org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full
        org-sticky-header-outline-path-separator " › "))

(use-package! org-appear
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autolinks t))


(after! org
  (setq org-agenda-files "~/org/agenda.org")
  ;;TODO set refile to only projects?
  ;; Set refile targets
  (setq org-refile-use-outline-path 'file              ; Show full paths for refiling
        org-outline-path-complete-in-steps nil)        ; Refile in a single go
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-completion-use-ido nil)
  (setq org-refile-targets '(("~/org/next.org" :level . 0)
                             ("~/org/work.org" :maxlevel . 2)
                             ("~/org/personal.org" :maxlevel . 3)
                             ("~/org/wiki/thesis.org" :maxlevel . 3)
                             ("~/org/import.org" :maxlevel . 2)
                             ("~/org/incubation.org" :maxlevel . 1)
                             ("~/org/someday.org" :maxlevel . 2)
                             ("~/org/read.org" :maxlevel . 2)
                             ("~/org/watch.org" :maxlevel . 2)))

  ;; Org-contacts
  (setq org-contacts-files '("~/org/contacts.org"))

  (setq org-blank-before-new-entry '((heading . nil)))

  (setq org-startup-folded 'fold)
  (setq org-id-link-to-org-use-id 'use-existing)

  (setq org-enable-org-journal-support t)
  (setq org-journal-dir "~/org/journal/")

  ;; Link types for org-mode
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mpg\\'" . "mpv %s")))

  (setq org-image-actual-width '420)
  ;; org-todo keywords with interactivity
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
          (type "PROJECT(p)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)

  ;; context tags
  (setq org-tag-alist '((:startgroup)
                            ;;; Contexts
                        ("@home" . ?h)
                        ;; ("@nathans" . ?n)
                        ;; ("@uni" . ?u)
                        ("@work" . ?u)
                        ("@errand" . ?e)
                        ("@shops" . ?s)
                        ("@onlineshop" . ?o)
                        ("@training" . ?T)
                        (:endgroup)
                        (:newline)
                            ;;; Tools
                        ("@phone" . ?p)
                        ("@computer" . ?c)
                        ;; ("@anywhere" . ?c)
                        (:newline)
                            ;;; Category
                        ("#email" . ?m)
                        ("#lowenergy" . ?l)
                        ("#translation" . ?t)
                        ("incremental" . ?t)
                        ;; ("web" . ?t)
                        ;; ("reading" . ?t)
                        (:newline)
                        ("WAITING" . ?W)
                        ("HOLD" . ?H)
                        ("CANCELLED" . ?C)
                        )))

(use-package! org-super-agenda
  ;; :commands (org-super-agenda-mode)
  ;; :init (advice-add #'org-super-agenda-mode :around #'doom-shut-up-a)
  :after org-agenda
  :init
  (setq org-super-agenda-groups
        `((:name "Schedule"
           :time-grid t)
          (:name "Habits"
           :habit t)
          (:name "Today"
           :time-grid t
           ;; :habit nil
           :discard (:header-regex "~/org/incubation.org"))
          (:name "Translation"
           :tag "#translation")
          (:name "Due today\n"
           :deadline today)
          (:name "Overdue"
           :deadline past)
          (:name "Due soon"
           :deadline future)
          (:name "Waiting\n"
           :todo "WAIT"
           :order 98)
          (:name "Scheduled earlier\n"
           :scheduled past)))
  :config
  (org-super-agenda-mode))

          (:name "Incubated - Hatched"
           ;; :scheduled today
           :discard (:not (:file-path "~/org/incubation.org")))
           ;; :file-path (,(concat org-directory "incubation.org")))

(use-package! org-ql
  :after org)

(after! org-agenda
  (setq org-agenda-custom-commands
        '(("c" "At computer or laptop" tags-todo "@computer"
           ((org-agenda-overriding-header "@Computer Tasks")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("e" "Emails to send" tags-todo "#email"
           ((org-agenda-overriding-header "Emails")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "DONE" "CANCELLED")))))
          ("h" "Tasks around the house" tags-todo "@home"
           ((org-agenda-overriding-header "@Home Tasks")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("t" "Translation (work) related tasks" tags-todo "#translation"
           ((org-agenda-overriding-header "Translation")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("o" "Online shopping" tags-todo "@onlineshop"
           ((org-agenda-overriding-header "@Online Shops")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "DONE" "CANCELLED")))))
          ("e" "Errands out and about" tags-todo "@errand"
           ((org-agenda-overriding-header "Errands")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("l" "Low energy tasks" tags-todo "#lowenergy"
           ((org-agenda-overriding-header "Low Energy")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ;; HACK Attempt to emulate rudimentary Supermemo IR stack in org
          ("i" "Incremental Reading stack"
           ((org-ql-block '(todo "READING")
                          ((org-ql-block-header "Incremental Reading Stack")))))
          ("p" "Projects list"
           ((org-ql-block '(todo "PROJECT")
                          ((org-ql-block-header "Test PROJECT list")))))
          ;; From https://github.com/alphapapa/org-ql/blob/master/examples.org
          ("n" "All NEXT actions"
           ((org-ql-block '(todo "NEXT")
                          ((org-ql-block-header "Next actions list")))))
          ("s" "Stuck Projects"
           ((org-ql-block '(and (todo "PROJECT")
                                (not (done))
                                (not (descendants (todo "NEXT")))
                                (not (descendants (scheduled))))
                          ((org-ql-block-header "Suck Projects")))))
          ;; List tasks without "PROJECT" parent
          ("O" "Orphaned Tasks"
           ((org-ql-block '(and (todo)
                                (path "personal.org"
                                      "work.org"
                                      "wiki/thesis.org")
                                (not (todo "PROJECT"))
                                (not (ancestors (todo "PROJECT")))))
            ((org-ql-block-header "Orphaned Tasks")))))))
(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    ;; (cfw:org-create-file-source "cal" "/path/to/cal.org" "Cyan")  ; other org source
    ;; (cfw:cal-create-source "Orange") ; diary source
    ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
   )))

(use-package! org-depend
  :after org
  :config
(defun r-org/org-insert-trigger ()
  "Automatically insert chain-find-next trigger when entry becomes NEXT"
  (cond ((equal org-state "NEXT")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((equal org-state "IN-PROGRESS")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((equal org-state "WAITING")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((not (member org-state org-done-keywords))
         (org-delete-property "TRIGGER")))))

(add-hook 'org-after-todo-state-change-hook 'r-org/org-insert-trigger)

;; Capture
;; TODO Transfer captures to DOCT
(after! org-capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; :ensure nil
  ;; :after org
  ;; :preface
  (defvar my/org-ledger-debitcard1-template "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Assets:Bank:Personal" "Template for personal debit card transactions with ledger.")

  (defvar my/org-ledger-debitcard2-template "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Assets:Bank:Business" "Template for business debit card transactions with ledger.")

  (defvar my/org-ledger-cash-template "%(org-read-date) * %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Assets:Cash:Wallet" "Template for cash transaction with ledger.")

  (defvar my/org-ledger-creditcard-template "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Liabilities:Credits Cards:CWB" "Template for credit card transaction with ledger.")
  :custom
  (setq org-default-notes-file "inbox.org"
        org-capture-templates
        `(("t" "Todo [inbox]" entry (file+headline "~/org/inbox.org" "Tasks")
           "** TODO %i%?")
          ("e" "email" entry (file+headline "~/org/inbox.org" "Tasks")
           "** TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("e" "email" entry (file+headline "~/org/inbox.org" "Tasks")
           "** TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("c" "Contacts" entry (file "~/org/contacts.org")
           "** %(org-contacts-template-name)\n:PROPERTIES:\n:ROLE: %^{Role}\n:COMPANY: %^{Company}\n:EMAIL: %^(org-contacts-template-email)\n:CELL:%^{Cellphone}\n:PHONE:%^{Phone}\n:WEBSITE: %^{Website}\n:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}\n:NOTE: %^{NOTE} \n:END:")
          ;; Ledger
          ("l" "Ledger")
          ("lp" "Personal Bank" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-debitcard1-template
           :empty-lines 1
           :immediate-finish t)
          ("lb" "Business Bank" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-debitcard2-template
           :empty-lines 1
           :immediate-finish t)
          ("lc" "CWB Credit Card" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-creditcard-template
           :empty-lines 1
           :immediate-finish t)
          ("lw" "Cash" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-cash-template
           :empty-lines 1
           :immediate-finish t))))

(use-package! ox-reveal
  :config
  ;; TODO GUIX path?
  ;; (setq org-reveal-root "file:///home/volk/Templates/reveal.js"))
  (setq org-reveal-root (concat "file://" abbreviated-home-dir "/Templates/reveal.js")))



;; PDF + synctex
(after! pdf-tools
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)
  ;; This allows for opening in an indirect buffer
  (setq pdf-sync-backward-display-action t)
  (setq pdf-sync-forward-display-action t)
(add-hook! 'pdf-outline-buffer-mode-hook #'doom-disable-line-numbers-h)
  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

   (after! pdf-view
     :config
     (define-pdf-cache-function pagelabels)

     (defun doom-modeline-update-pdf-pages ()
       "Update PDF pages."
       (setq doom-modeline--pdf-pages
             (format "  [P%s:%s/%s] "
                     (eval '(nth (1- (pdf-view-current-page))
                                 (pdf-cache-pagelabels)))
                     (number-to-string (pdf-view-current-page))
                     (number-to-string (pdf-cache-number-of-pages))))))
;; org-ref
(use-package! org-ref
  :config
  (setq reftex-default-bibliography "~/Documents/LaTeX/uni.bib"
        org-ref-default-bibliography '("~/Documents/LaTeX/uni.bib")
        org-ref-pdf-directory "~/Documents/PDF/"
        org-ref-bibliography-notes "~/org/org-brain/article-notes"
        bibtex-completion-bibliography "~/Documents/LaTeX/uni.bib"
        bibtex-completion-library-path "~/Documents/PDF"
        bibtex-completion-notes-path "~/org/org-brain/article-notes"
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-symbol "✎"
        bibtex-completion-additional-search-fields '(keywords)
;; I should improve the formatting of this:
        bibtex-completion-notes-template-one-file
        (format
         "\n* ${author}${year}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :Keywords: ${keywords}\n  :YEAR: ${year}\n  :END:\n\n  - cite:${=key=}")
        doi-utils-open-pdf-after-download nil
        org-ref-note-title-format "* (%y) %t\n  :PROPERTIES:\n  :Custom_ID: %k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n  :END:")
        )
;; bib-library "~/Documents/LaTeX/uni.bib"
(after! 'ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass[12]{article}
        [NO-DEFAULT-PACKAGES]
        [PACKAGES]
        [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-pdf-process
  '("lualatex -shell-escape -interaction nonstopmode %f"
    "lualatex -shell-escape -interaction nonstopmode %f"))

(setq luamagick '(luamagick :programs ("lualatex" "convert")
       :description "pdf > png"
       :message "you need to install lualatex and imagemagick."
       :use-xcolor t
       :image-input-type "pdf"
       :image-output-type "png"
       :image-size-adjust (1.0 . 1.0)
       :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

(add-to-list 'org-preview-latex-process-alist luamagick)

(setq org-preview-latex-default-process 'luamagick)

;; (use-package! org-expex)


(use-package! polymode)

;; org-brain
(use-package! org-brain
  :defer t

  :init
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length 24
        org-brain-include-file-entries t
        org-brain-backlink t)
  (setq org-brain-path "~/org/org-brain")

  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)

  (set-popup-rule! "^\\*org-brain"
    :side 'right :size 1.00 :select t :ttl nil)

  (when (featurep! :editor evil +everywhere)
    ;; TODO Make a proper evil keybind scheme for org-brain
    ;; REVIEW This should be handled upstream by evil-collection
    (set-evil-initial-state!
      '(org-brain-visualize-mode
        org-brain-select-map
        org-brain-move-map
        org-brain-polymode-map)
      'normal)
    (defun +org--evilify-map (map)
      (let (keys)
        (map-keymap (lambda (event function)
                      (push function keys)
                      (push (vector event) keys))
                    map)
        (apply #'evil-define-key* 'normal map keys)))

    (+org--evilify-map org-brain-visualize-mode-map)
    (+org--evilify-map org-brain-select-map)
    (+org--evilify-map org-brain-move-map)
    (after! polymode
      (+org--evilify-map org-brain-polymode-map)))

  (defun +popup-toggle-brain ()
    (interactive)
    (let ((+popup--inhibit-transient t))
      (cond ((+popup-windows) (+popup/close-all t))
            ((display-buffer (get-buffer "*org-brain*"))))))

  (setq org-id-track-globally t
        org-id-locations-file "~/org/.org-id-locations")

  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (cl-pushnew '("b" "Brain" plain (function org-brain-goto-end)
                "* %i%?" :empty-lines 1)
              org-capture-templates
              :key #'car :test #'equal)
  ;; (define-key org-brain-visualize-mode-map (kbd "C-l") #'link-hint-open-link)
  (map! :map org-brain-visualize-mode-map "C-o" #'link-hint-open-link)

  (use-package! org-cliplink)
  (defun org-brain-cliplink-resource ()
    "Add a URL from the clipboard as an org-brain resource.
    Suggest the URL title as a description for resource."
    (interactive)
    (let ((url (org-cliplink-clipboard-content)))
      (org-brain-add-resource
       url
       (org-cliplink-retrieve-title-synchronously url)
       t)))

  (map! :map org-brain-visualize-mode-map "L" #'org-brain-cliplink-resource)
  (add-hook 'org-brain-after-visualize-hook #'visual-line-mode)
  ;; Prettier line drawing

  (defface aa2u-face '((t . nil))
    "Face for aa2u box drawing characters")
  (advice-add #'aa2u-1c :filter-return
              (lambda (str) (propertize str 'face 'aa2u-face)))
  (defun aa2u-org-brain-buffer ()
    (let ((inhibit-read-only t))
      (make-local-variable 'face-remapping-alist)
      (add-to-list 'face-remapping-alist
                   '(aa2u-face . org-brain-wires))
      (ignore-errors (aa2u (point-min) (point-max)))))
  (with-eval-after-load 'org-brain
    (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))

  (defun org-brain-insert-resource-icon (link)
    "Insert an icon, based on content of org-mode LINK."
    (insert (format "%s "
                    (cond ((string-prefix-p "brain:" link)
                           (all-the-icons-fileicon "brain"))
                          ((string-prefix-p "info:" link)
                           (all-the-icons-octicon "info"))
                          ((string-prefix-p "help:" link)
                           (all-the-icons-material "help"))
                          ((string-prefix-p "http" link)
                           (all-the-icons-icon-for-url link))
                          (t
                           (all-the-icons-icon-for-file link))))))

  (with-eval-after-load 'all-the-icons
    (add-hook 'org-brain-after-resource-button-functions
              'org-brain-insert-resource-icon))

  ;; Setup org-expiry and define a org-agenda function to compare timestamps
  (use-package! org-expiry
    :after org-brain
    :config
    (setq org-expiry-inactive-timestamps t)
    (defun org-expiry-created-comp (a b)
      "Compare `org-expiry-created-property-name' properties of A and B."
      (let ((ta (ignore-errors
                  (org-time-string-to-seconds
                   (org-entry-get (get-text-property 0 'org-marker a)
                                  org-expiry-created-property-name))))
            (tb (ignore-errors
                  (org-time-string-to-seconds
                   (org-entry-get (get-text-property 0 'org-marker b)
                                  org-expiry-created-property-name)))))
        (cond ((if ta (and tb (< ta tb)) tb) -1)
              ((if tb (and ta (< tb ta)) ta) +1))))

    ;; Add CREATED property when adding a new org-brain headline entry
    (add-hook 'org-brain-new-entry-hook #'org-expiry-insert-created)

    ;; Finally add a function which lets us watch the entries chronologically
    (defun org-brain-timeline ()
      "List all org-brain headlines in chronological order."
      (interactive)
      (let ((org-agenda-files (org-brain-files))
            (org-agenda-cmp-user-defined #'org-expiry-created-comp)
            (org-agenda-sorting-strategy '(user-defined-down)))
        (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))))

(after! org-brain
  :init

  (load! "+helm-org-brain")

  :config
;; (map! :map helm-org-brain-map
;;         "C-c o" #'helm-org-brain-switch-node-other-window)

  (map! :map org-mode-map
      ;; "C-c a"         #'org-agenda
       (:prefix ("C-c b" . "brain")
        "a"            #'org-brain-agenda
        "v"            #'org-brain-visualize)))

;; (defun helm-org-rifle-brain ()
;;   ;; "Rifle files in `org-brain-path'.\"
;;   (interactive)
;;   (helm-org-rifle-directories (list org-brain-path)))


;; org-noter
;; org-noter + org-brain
;; https://github.com/Kungsgeten/org-brain#org-noter
(add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
(defun org-brain-open-org-noter (entry)
  "Open `org-noter' on the ENTRY. If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (org-with-point-at (org-brain-entry-marker entry)
    (org-noter)))

;;    (define-key org-brain-visualize-mode-map (kbd "\C-c n") 'org-brain-open-org-noter)
(map! :map org-brain-visualize-mode-map "\C-c n" #'org-brain-open-org-noter)

;; org-pomodoro
(use-package! org-pomodoro
  :after org
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))

;; (use-package! org-drill
;;   :after org
;;   :config
;;   (setq org-drill-scope (quote directory)))

;; (org-drill "~/org/article-notes/article-index.org")

(use-package! org-noter
  :after org
  :config
  ;; Your org-noter config ........
  (require 'org-noter-pdftools)
  (setq org-noter-notes-search-path '("~/org/org-brain/article-notes/")))

(use-package! org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  (setq org-noter-pdftools-insert-content-heading nil)
  (setq org-noter-pdftools-use-org-id nil) ;; this interferes with org-brain
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(load! "+ir.el")

(use-package! org-web-tools)
(use-package! org-web-tools-archive)

;; (use-package! org-gcal
;;   :after org
;;   :init
;;   ;; Currently not working https://github.com/kidd/org-gcal.el/issues/58
;;   ;; https://console.cloud.google.com/apis/credentials/
;;   (setq org-gcal-client-id (password-store-get "secrets/org-gcal-client-id")
;;         org-gcal-client-secret (password-store-get "secrets/org-gcal-client-secret"))

;;   (setq org-gcal-file-alist '(("otoo.danieru@gmail.com" . "~/org/danieru-cal.org"))))

(load! "+org-media-note.el")
(load! "+org-protocol.el")
