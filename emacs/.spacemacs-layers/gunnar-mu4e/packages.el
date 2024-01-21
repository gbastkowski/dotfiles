(defconst gunnar-mu4e-packages '(mu4e))

(defun gunnar-mu4e/post-init-mu4e ()
  (setq mu4e-installation-path          "/opt/homebrew/Cellar/mu/1.10.8/share/emacs/site-lisp/mu/mu4e/"
        mu4e-use-maildirs-extension     t
        mu4e-enable-async-operations    t
        mail-user-agent                 'mu4e-user-agent
        mu4e-get-mail-command           "offlineimap"         ;; allow for updating mail using 'U' in the main view:
        starttls-use-gnutls             t
        message-send-mail-function      'smtpmail-send-it
        message-kill-buffer-on-exit     t
        ;; Stop creating backups and lock files
        create-lockfiles                nil
        backup-directory-alist          '((".*" . "~/.Trash"))

        mu4e-compose-signature-auto-include t
        mu4e-enable-notifications       nil
        mu4e-enable-mode-line           nil
        mu4e-update-interval            nil ; seconds

        mu4e-maildir-shortcuts          '()
        ;; mu4e-maildir-shortcuts          '((:maildir "/gunnar.bastkowski@mobimeo.com/INBOX"              :key ?i)
        ;;                                   (:maildir "/gunnar.bastkowski@mobimeo.com/[Gmail].Sent Mail"  :key ?s)
        ;;                                   (:maildir "/gunnar.bastkowski@mobimeo.com/[Gmail].All Mail"   :key ?a))

        mu4e-contexts
        `(,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda () (progn (mu4e-message "Entering Gmail context")))
            ;; :leave-func (lambda () (progn (mu4e-message "Leaving Gmail context")))
            ;; we match based on the contact-fields of the message
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/gunnar.bastkowski@gmail.com" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address              . "gunnar.bastkowski@gmail.com")
                    (user-full-name	                . "Gunnar Bastkowski")
                    (message-user-organization      . "Home")
                    (mu4e-compose-signature         . (concat "Gunnar Bastkowski"))

                    (mu4e-drafts-folder             . "/gunnar.bastkowski@gmail.com/[Gmail].Drafts")
                    (mu4e-refile-folder             . "/gunnar.bastkowski@gmail.com/[Gmail].All Mail")
                    (mu4e-sent-folder               . "/gunnar.bastkowski@gmail.com/[Gmail].Sent Mail")
                    (mu4e-trash-folder              . "/gunnar.bastkowski@gmail.com/[Gmail].Trash")
                    (mu4e-sent-messages-behavior    . 'delete)

                    (mu4e-maildir-shortcuts         . ((:maildir "/gunnar.bastkowski@gmail.com/INBOX"                                         :key ?i)
                                                       (:maildir "/gunnar.bastkowski@gmail.com/[Gmail].Sent Mail"                             :key ?s)
                                                       (:maildir "/gunnar.bastkowski@gmail.com/[Gmail].All Mail"                              :key ?a)))

                    (mu4e-bookmarks                 . ((:name "Inbox"                 :query "maildir:/gunnar.bastkowski@gmail.com/INBOX"     :key ?i   :favorite t)
                                                       (:name "Unread messages"       :query "flag:unread AND NOT flag:trashed"               :key 117)
                                                       (:name "Today's messages"      :query "date:today..now"                                :key 116)
                                                       (:name "Last 7 days"           :query "date:7d..now"                                   :key 119  :hide-unread t)
                                                       (:name "Messages with images"  :query "mime:image/*"                                   :key 112)))

                    (smtpmail-starttls-credentials  . '(("smtp.gmail.com" 587 nil nil)))
                    (smtpmail-auth-credentials      . '(("smtp.gmail.com" 587 "gunnar.bastkowski@gmail.com" nil)))
                    (smtpmail-default-smtp-server   . "smtp.gmail.com")
                    (smtpmail-smtp-server           . "smtp.gmail.com")
                    (smtpmail-smtp-service          . 587)
                  ))
          ,(make-mu4e-context
            :name "Mobimeo"
            :enter-func (lambda () (progn (mu4e-message "Entering Mobimeo context")))
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/gunnar.bastkowski@mobimeo.com" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address              . "gunnar.bastkowski@mobimeo.com")
                    (user-full-name	                . "Gunnar Bastkowski")
                    (message-user-organization      . "Home")
                    (mu4e-compose-signature         (concat "Gunnar Bastkowski"))

                    (mu4e-drafts-folder             . "/gunnar.bastkowski@mobimeo.com/[Gmail].Drafts")
                    (mu4e-refile-folder             . "/gunnar.bastkowski@mobimeo.com/[Gmail].All Mail")
                    (mu4e-sent-folder               . "/gunnar.bastkowski@mobimeo.com/[Gmail].Sent Mail")
                    (mu4e-trash-folder              . "/gunnar.bastkowski@mobimeo.com/[Gmail].Trash")
                    (mu4e-sent-messages-behavior    . 'delete)

                    (mu4e-maildir-shortcuts         . ((:maildir "/gunnar.bastkowski@mobimeo.com/INBOX"                                       :key ?i)
                                                       (:maildir "/gunnar.bastkowski@mobimeo.com/[Gmail].Sent Mail"                           :key ?s)
                                                       (:maildir "/gunnar.bastkowski@mobimeo.com/[Gmail].All Mail"                            :key ?a)))

                    (mu4e-bookmarks                 . ((:name "Inbox"                 :query "maildir:/gunnar.bastkowski@mobimeo.com/INBOX"   :key ?i   :favorite t)
                                                       (:name "Unread messages"       :query "flag:unread AND NOT flag:trashed"               :key 117)
                                                       (:name "Today's messages"      :query "date:today..now"                                :key 116)
                                                       (:name "Last 7 days"           :query "date:7d..now"                                   :key 119  :hide-unread t)
                                                       (:name "Messages with images"  :query "mime:image/*"                                   :key 112)))

                    (smtpmail-starttls-credentials  . '(("smtp.gmail.com" 587 nil nil)))
                    (smtpmail-auth-credentials      . '(("smtp.gmail.com" 587 "gunnar.bastkowski@mobimeo.com" nil)))
                    (smtpmail-default-smtp-server   . "smtp.gmail.com")
                    (smtpmail-smtp-server           . "smtp.gmail.com")
                    (smtpmail-smtp-service          . 587)
                  ))
          ,(make-mu4e-context
            :name "bastkowski.name"
            :enter-func (lambda () (mu4e-message "Entering bastkowski.name context"))
            ;; :leave-func (lambda () (mu4e-message "Leaving bastkowski.name context"))
            ;; we match based on the contact-fields of the message
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/gunnar@bastkowski.name" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address              . "gunnar@bastkowski.name")
                    (user-full-name	                . "Gunnar Bastkowski")
                    (message-user-organization      . "Home")
                    (mu4e-compose-signature         (concat "Gunnar Bastkowski"))

                    (mu4e-drafts-folder             . "/gunnar@bastkowski.name/Drafts")
                    (mu4e-refile-folder             . "/gunnar@bastkowski.name/Archive")
                    (mu4e-sent-folder               . "/gunnar@bastkowski.name/Sent")
                    (mu4e-trash-folder              . "/gunnar@bastkowski.name/Trash")
                    (mu4e-sent-messages-behavior    . 'delete)

                    (mu4e-maildir-shortcuts         . ((:maildir "/gunnar@bastkowski.name/INBOX"                                              :key ?i)))

                    (mu4e-bookmarks                 . ((:name "Inbox"                 :query "maildir:/gunnar@bastkowski.name/INBOX"          :key ?i   :favorite t)
                                                       (:name "Unread messages"       :query "flag:unread AND NOT flag:trashed"               :key 117)
                                                       (:name "Today's messages"      :query "date:today..now"                                :key 116)
                                                       (:name "Last 7 days"           :query "date:7d..now"                                   :key 119  :hide-unread t)
                                                       (:name "Messages with images"  :query "mime:image/*"                                   :key 112)))

                    (smtpmail-starttls-credentials  . '(("smtp.bastkowski.name" 465 nil nil)))
                    (smtpmail-auth-credentials      . '(("smtp.bastkowski.name" 465 "gunnar@mobimeo.com" nil)))
                    (smtpmail-default-smtp-server   . "smtp.bastkowski.name")
                    (smtpmail-smtp-server           . "smtp.bastkowski.name")
                    (smtpmail-smtp-service          . 465)
                  ))
          ))
  )

(defun gunnar/make-mu4e-gmail-context (context-name mail-address organisation signature)
  (let ((dir-name (concat "/" mail-address)))
    (make-mu4e-context
     :name context-name
     :match-func `(lambda (msg) (when msg (string-prefix-p dir-name (mu4e-message-field msg :maildir))))
     :vars ((user-mail-address              . ,mail-address)
            (user-full-name	                . "Gunnar Bastkowski")
            (message-user-organization      . ,organisation)
            (mu4e-compose-signature         . ,signature)

            (mu4e-drafts-folder             . ,(concat dir-name "/[Gmail].Drafts"))
            (mu4e-refile-folder             . ,(concat dir-name "/[Gmail].All Mail"))
            (mu4e-sent-folder               . ,(concat dir-name "/[Gmail].Sent Mail"))
            (mu4e-trash-folder              . ,(concat dir-name "/[Gmail].Trash"))
            (mu4e-sent-messages-behavior    . 'delete)

            (mu4e-maildir-shortcuts         . ,((:maildir (concat dir-name "/INBOX")                                         :key ?i)
                                                (:maildir (concat dir-name "/[Gmail].Sent Mail")                             :key ?s)
                                                (:maildir (concat dir-name "/gunnar.bastkowski@gmail.com/[Gmail].All Mail")  :key ?a)))

            (mu4e-bookmarks                 . ,((:name "Inbox"                 :query (concat "maildir:" dir-name "/INBOX")  :key ?i   :favorite t)
                                                (:name "Unread messages"       :query "flag:unread AND NOT flag:trashed"     :key 117)
                                                (:name "Today's messages"      :query "date:today..now"                      :key 116)
                                                (:name "Last 7 days"           :query "date:7d..now"                         :key 119  :hide-unread t)
                                                (:name "Messages with images"  :query "mime:image/*"                         :key 112)))

            (smtpmail-starttls-credentials  . '(("smtp.gmail.com" 587 nil nil)))
            ;; (smtpmail-auth-credentials      . '(("smtp.gmail.com" 587 "gunnar.bastkowski@gmail.com" nil)))
            (smtpmail-default-smtp-server   . "smtp.gmail.com")
            (smtpmail-smtp-server           . "smtp.gmail.com")
            (smtpmail-smtp-service          . 587)))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; (setq mu4e-compose-context-policy nil)
