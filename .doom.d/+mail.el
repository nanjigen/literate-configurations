;;; $DOOMDIR/+mail.el -*- lexical-binding: t; -*-

;; (org-link-set-parameters "mu4e" :follow #'org-mu4e-open :store
;; #'org-mu4e-store-link)

(use-package! org-mu4e
  :after mu4e)
;; configure email


;; (map! :localleader
;;       :map mu4e-compose-mode-map
;;       :desc "Attach file" "a" 'mml-attach-file
;;       :desc "Send message" "s" 'message-send-and-exit)

(use-package! mu4e
  :defer 20
  :init
  (setq mu4e-maildir "~/.mail"
        mu4e-get-mail-command "mbsync -Va"
        mu4e-update-interval (* 10 60)
        message-kill-buffer-on-exit t
        ;; mu4e-headers-auto-update nil ; no refresh cos lose current filter
        mu4e-change-filenames-when-moving t ; Preferred for mbsync according to man page.)
)
        ;; Don't bother me with context on startup
        ;; mu4e-context-policy nil

        ;; Attachments
        ;; Use Ivy for mu4e completions (maildir folders, etc)
        ;; (setq mu4e-completing-read-function #'ivy-completing-read)
        ;; mu4e-attachment-dir "~/Downloads"
        ;; mu4e-save-multiple-attachments-without-asking t
        ;; mu4e-compose-signature-auto-include nil)

  ;; Display
  (setq message-yank-prefix "")
  (setq message-yank-cited-prefix "")
  (setq message-yank-empty-prefix "")
  (setq message-indentation-spaces 4)
  (setq message-cite-reply-position 'above)
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

  :config

  ;; SMTP stuff
  (setq send-mail-function 'smtpmail-send-it
        ;; message-send-mail-function 'message-send-mail-with-sendmail
        ;; substitute sendmail with msmtp
        ;; sendmail-program "msmtp"
        ;; allow setting account through email header
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil t)

  ;; enable format=flowed
  ;; - mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
  ;; - each paragraph is a single long line; at sending, emacs will add the
  ;;   special line continuation characters.
  ;; - also see visual-line-fringe-indicators setting below
  (setq mu4e-compose-format-flowed t)
  ;; because it looks like email clients are basically ignoring format=flowed,
  ;; let's complicate their lives too. send format=flowed with looong lines. :)
  ;; https://www.ietf.org/rfc/rfc2822.txt
  (setq fill-flowed-encode-column 998)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  (setq mu4e-bookmarks (list (make-mu4e-bookmark
                              :name "Uni Inbox"
                              :query (concat "maildir:/unsw/INBOX OR "
                                             "maildir:/zmail/INBOX")
                              :key ?i)
                             (make-mu4e-bookmark
                              :name "Work Inbox"
                              :query "maildir:/translating/INBOX"
                              :key ?w)
                             ;; (make-mu4e-bookmark
                             ;;  :name "Work Unread"
                             ;;  :query "maildir:/Work/INBOX AND flag:unread"
                             ;;  :key ?q)
                             ;; (make-mu4e-bookmark
                             ;;  :name "All Sent"
                             ;;  :query (concat "\"maildir:/Gmail/[Gmail].Sent Mail\" OR "
                             ;;                 "maildir:/Fastmail/Sent OR "
                             ;;                 "\"maildir:/Work/[Gmail].Sent Mail\"")
                             ;;  :key ?s)
                             (make-mu4e-bookmark
                              :name "Personal Unread"
                              :query (concat "maildir:/gmail/INBOX AND flag:unread OR "
                                             "maildir:/personal/INBOX AND flag:unread")
                              :key ?p))
        ;; (make-mu4e-bookmark
        ;;  :name "Personal Archived"
        ;;  :query (concat "maildir:/Gmail/[Gmail].Archive OR "
        ;;                 "maildir:/Fastmail/Archive")
        ;; :key ?a)
        ))

(use-package! auth-source-pass)
(auth-source-pass-enable)

(after! mu4e
  :config
  (setq mail/personal (auth-source-pass-get "user" "email/personal/hotmail")
        mail/translating (auth-source-pass-get "user" "email/work/translating")
        mail/ad (auth-source-pass-get "user" "email/uni/ad")
        mail/student (auth-source-pass-get "user" "email/uni/student")
        domain/personal (auth-source-pass-get "domain" "email/personal/hotmail")
        domain/translating (auth-source-pass-get "domain" "email/work/translating")
        domain/student (auth-source-pass-get "domain" "email/uni/student")))

(after! mu4e
  :config

  (setq mu4e-contexts
    (list
      (make-mu4e-context
    :name "UNSW"
    :enter-func (lambda () (mu4e-message "Switch to the uni context"))
    :match-func (lambda (msg)
        (when msg
          (mu4e-message-contact-field-matches msg :to (symbol-value 'mail/student))))
      :vars `((user-mail-address . ,mail/student)
              (mu4e-sent-folder . "/unsw/Sent")
              (mu4e-drafts-folder . "/unsw/Drafts")
              (mu4e-trash-folder . "/unsw/Trash")
              (mu4e-refile-folder . "/unsw/Archive")
              (mu4e-sent-messages-behavior . sent)
              (smtpmail-smtp-user . ,mail/ad)
              (smtpmail-local-domain . ,domain/student)
              (smtpmail-default-smtp-server . "smtp.office365.com")
              (smtpmail-smtp-server . "smtp.office365.com")
              (smtpmail-smtp-service . 587)))
        (make-mu4e-context
      :name "personal"
      :enter-func (lambda () (mu4e-message "Switch to the personal context"))
      :match-func (lambda (msg)
        (when msg
          (mu4e-message-contact-field-matches msg :to (symbol-value 'mail/personal))))
      :vars `((user-mail-address . ,mail/personal)
              (mu4e-sent-folder . "/personal/Sent")
              (mu4e-drafts-folder . "/personal/Drafts")
              (mu4e-trash-folder . "/personal/Trash")
              (mu4e-refile-folder . "/personal/Archive")
              (mu4e-sent-messages-behavior . sent)
              ;; (message-send-mail-function . smtpmail-send-it)
              (smtpmail-stream-type . starttls)
              (smtpmail-smtp-user . ,mail/personal) ;
              ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
              (smtpmail-default-smtp-server . "smtp.office365.com")
              (smtpmail-smtp-server . "smtp.office365.com")
              (smtpmail-smtp-service . 587)))
        (make-mu4e-context
          :name "translating"
          :enter-func (lambda () (mu4e-message "Switch to the translation context"))
          :match-func (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg :to (symbol-value 'mail/translating))))
          :vars `((user-mail-address . ,mail/translating)
                  (mu4e-sent-folder . "/translating/Sent")
                  (mu4e-drafts-folder . "/translating/Drafts")
                  (mu4e-trash-folder . "/translating/Trash")
                  (mu4e-refile-folder . "/translating/Archive")
                  (mu4e-sent-messages-behavior . sent)
                  ;; (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-stream-type . ssl)
                  (smtpmail-smtp-user . ,mail/translating)
                  (smtpmail-local-domain . ,domain/translating)
                  (smtpmail-default-smtp-server . "smtp.fastmail.com")
                  (smtpmail-smtp-server . "smtp.fastmail.com")
                  (smtpmail-smtp-service . 465)))))
  (mu4e t))
