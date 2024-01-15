(defconst gunnar-mu4e-packages '(mu4e))

(defun gunnar-mu4e/post-init-mu4e ()

  (setq mail-user-agent 'mu4e-user-agent

        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash"
        ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
        mu4e-sent-messages-behavior 'delete
        mu4e-maildir-shortcuts '( (:maildir "/INBOX"              :key ?i)
                                  (:maildir "/[Gmail].Sent Mail"  :key ?s)
                                  (:maildir "/[Gmail].Trash"      :key ?;TODO: )
                                  (:maildir "/[Gmail].All Mail"   :key ?a)))

        ;; allow for updating mail using 'U' in the main view:
        mu4e-get-mail-command "offlineimap"

        user-mail-address "gunnar.bastkowski@gmail.com"
        user-full-name  "Gunnar Bastkowski"
        mu4e-compose-signature (concat "Gunnar Bastkowski")

        starttls-use-gnutls t

        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "gunnar.bastkowski@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587

        message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t
        ;; Stop creating backups and lock files
        create-lockfiles nil
        backup-directory-alist '((".*" . "~/.Trash"))

        mu4e-compose-signature-auto-include t
        mu4e-enable-notifications t
        mu4e-enable-mode-line t
        mu4e-update-interval 120 ; seconds
        )

  (add-to-list 'mu4e-bookmarks
               ;; ':favorite t' i.e, use this one for the modeline
               '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t)))
