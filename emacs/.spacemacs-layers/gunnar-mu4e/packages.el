(defconst gunnar-mu4e-packages '(mu4e))

(defun gunnar-mu4e/post-init-mu4e ()
  (setq mu4e-installation-path          "/opt/homebrew/Cellar/mu/1.10.8/share/emacs/site-lisp/mu/mu4e/"
        mu4e-use-maildirs-extension     t
        mu4e-enable-async-operations    t
        mail-user-agent                 'mu4e-user-agent
        mu4e-get-mail-command           "offlineimap"         ;; allow for updating mail using 'U' in the main view:
        starttls-use-gnutls             t
        )
  (setq message-send-mail-function      'smtpmail-send-it
        message-kill-buffer-on-exit     t
        ;; Stop creating backups and lock files
        create-lockfiles                nil
        backup-directory-alist          '((".*" . "~/.Trash"))

        mu4e-compose-signature-auto-include t
        mu4e-enable-notifications       nil
        mu4e-enable-mode-line           nil
        mu4e-update-interval            nil ; seconds

        mu4e-maildir-shortcuts  '((:maildir "/INBOX"              :key ?i)
                                  (:maildir "/[Gmail].Sent Mail"  :key ?s)
                                  (:maildir "/[Gmail].All Mail"   :key ?a))

        ;; ':favorite t' i.e, use this one for the modeline
        mu4e-bookmarks          '((:name "Inbox"                 :query "maildir:/inbox"                   :key ?i   :favorite t)
                                  (:name "Unread messages"       :query "flag:unread AND NOT flag:trashed" :key 117)
                                  (:name "Today's messages"      :query "date:today..now"                  :key 116)
                                  (:name "Last 7 days"           :query "date:7d..now"                     :key 119  :hide-unread t)
                                  (:name "Messages with images"  :query "mime:image/*"                     :key 112))
        )
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "gunnar.bastkowski@gmail.com"
            :enter-func (lambda () (mu4e-message "Entering Gmail context"))
            :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
            ;; we match based on the contact-fields of the message
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg :to "gunnar.bastkowski@gmail.com")))
            :vars '((user-mail-address              . "gunnar.bastkowski@gmail.com")
                    (user-full-name	                . "Gunnar Bastkowski")
                    (message-user-organization      . "Home")
                    (mu4e-compose-signature         (concat "Gunnar Bastkowski"))

                    (mu4e-drafts-folder             . "/[Gmail].Drafts")
                    (mu4e-sent-folder               . "/[Gmail].Sent Mail")
                    (mu4e-trash-folder              . "/[Gmail].Trash")
                    (mu4e-sent-messages-behavior    . 'delete)

                    (smtpmail-starttls-credentials  . '(("smtp.gmail.com" 587 nil nil)))
                    (smtpmail-auth-credentials      . '(("smtp.gmail.com" 587 "gunnar.bastkowski@gmail.com" nil)))
                    (smtpmail-default-smtp-server   . "smtp.gmail.com")
                    (smtpmail-smtp-server           . "smtp.gmail.com")
                    (smtpmail-smtp-service          . 587)
                  ))))
  )


  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; (setq mu4e-compose-context-policy nil)
