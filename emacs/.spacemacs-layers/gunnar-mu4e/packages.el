(defconst gunnar-mu4e-packages '(mu4e))

(defun gunnar-mu4e/post-init-mu4e ()
  (setq mu4e-maildir "~/.mail/gmail"
        user-full-name "Gunnar Bastkowski"
        user-mail-address "gunnar.bastkowski@enfore.com"
        mu4e-inbox-folder "/Inbox"
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder "/Sent"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "mbsync gmail && mu index -m ~/.mail/gmail"
        mu4e-update-interval 120 ; seconds
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-enable-notifications t
        mu4e-enable-mode-line t
        ;; don't save message to Sent Messages, GMail/IMAP will take care of this
        mu4e-sent-messages-behavior 'delete
        mu4e-maildir-shortcuts '(("/Inbox" . ?i)))

  (with-eval-after-load 'mu4e-alert (mu4e-alert-set-default-style 'notifier))
  )
