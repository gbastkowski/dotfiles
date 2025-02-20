;;; openai-playground.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Gunnar Bastkowski
;;
;; Author: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Maintainer: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Created: February 20, 2025
;; Modified: February 20, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gunnar/openai-playground
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(provide 'openai-playground)
;;; openai-playground.el ends here

(defun chatgpt-send-request (start end)
  "Align code by equal signs using ChatGPT's chat completions."
  (interactive "r")
  (let* ((code-text (buffer-substring-no-properties start end))
         (api-key (password-store-get "private/openai/emacs-api-key"))
         (openai-project "proj_iowmSorSlumW84As4VhYe9mq")
         (openai-org "org-VujiJPSpiJilwgH1K9z5pnnL"))
    (request "https://api.openai.com/v1/chat/completions"
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(concat "Bearer " api-key)))
      :data (json-encode `(
                           ("model" . ,"gpt-4o-mini")
                           ("messages" . ((("role" . "user")
                                           ("content" . ,(concat "Say hello\n\n" code-text)))))))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "Done: %s" (assoc-default 'form data))))
      ;; (cl-function
      ;;           (lambda (&key data &allow-other-keys)
      ;;             (when data
      ;;               (let* ((responses (gethash "choices" data))
      ;;                      (latest-response (aref responses 0))
      ;;                      (text (gethash "message" (aref (gethash "messages" latest-response) 0))))
      ;;                 (delete-region start end)
      ;;                 (insert text)))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Failed to align code: %S" error-thrown))))))

(request "https://api.openai.com/v1/chat/completions"
  :type "POST"
  :headers `(("Content-Type" . "application/json")
             ("Authorization" . ,(concat "Bearer " (password-store-get "private/openai/emacs-api-key"))))
  :data (json-encode `(
                       ("model" . ,"gpt-4o-mini")
                       ("messages" . ((("role" . "user")
                                       ("content" . ,(concat "Say hello\n\n" "Gunnar")))))))
  ;; :parser 'buffer-string
  :parser 'json-read
  :success
  (cl-function (lambda (&key data &allow-other-keys)
                 (when data
                   (with-current-buffer (get-buffer-create "*request demo*")
                     (erase-buffer)
                     (insert (format "%s"
                                     (assoc 'message (aref (cdr (assoc 'choices data)) 0))))
                     (pop-to-buffer (current-buffer))))))
  :error
  (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                 (message "Got error: %S" error-thrown)))
  :complete (lambda (&rest _) (message "Finished!"))
  :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                 (418 . (lambda (&rest _) (message "Got 418.")))))
