;;; openai-playground.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Gunnar Bastkowski
;;
;; Author: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Maintainer: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Created: February 20, 2025
;; Modified: February 20, 2025
;; Version: 0.0.1
;; Keywords:  hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gbastkowski/openai-playground
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(provide 'openai-playground)

(defconst openai-completion-api   "https://api.openai.com/v1/chat/completions")
(defconst openai-project          "proj_iowmSorSlumW84As4VhYe9mq")
(defconst openai-org              "org-VujiJPSpiJilwgH1K9z5pnnL")
(defconst openai-completion-model "gpt-4o-mini")
(defconst openai-headers          `(("Content-Type"        . "application/json")
                                    ("Authorization"       . ,(concat "Bearer " (password-store-get "private/openai/emacs-api-key")))
                                    ("OpenAI-Organization" . ,openai-org)
                                    ("OpenAI-Project"      . ,openai-project)))

(defun chatgpt-send-request (start end)
  "Align code by equal signs using ChatGPT's chat completions."
  (interactive "r")
  (let* ((code-text (buffer-substring-no-properties start end))
         (api-key (password-store-get "private/openai/emacs-api-key"))
         (openai-project "proj_iowmSorSlumW84As4VhYe9mq")
         (openai-org "org-VujiJPSpiJilwgH1K9z5pnnL"))
    (request "https://api.openai.com/v1/chat/completions"
      :type "POST"
      :headers `(("Content-Type"        . "application/json")
                 ("Authorization"       . ,(concat "Bearer " api-key))
                 ("OpenAI-Organization" . ,openai-org)
                 ("OpenAI-Project"      . ,openai-project))
      :data (json-encode `(("model"     . ,"gpt-4o-mini")
                           ("messages"  . ((("role" . "user")
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

(defun openai-completion-request-create-body (messages &optional model)
  (let ((model (or model openai-completion-model))
        (messages (mapcar (lambda (pair)
                            `(("role"    . ,(symbol-name (car pair)))
                              ("content" . ,(cdr pair))))
                          messages)))
    (json-encode `(("model"    . ,model)
                   ("messages" . ,messages)))))

(defun openai-completion-response-extract-messages (data)
  (let* ((choices (alist-get 'choices data))
        (first-choice (aref choices 0))
        (message (alist-get 'message first-choice))
        (content (alist-get 'content message)))
    (format "%s\n" content)))

(defun openai-completion-log-to-buffer (data &optional pop)
  (with-current-buffer (get-buffer-create "*openai completion log*")
    (goto-char (point-max))
    (insert (openai-completion-response-extract-messages data))
    (when pop (pop-to-buffer (current-buffer)))))

(defun openai-completion-send-region (fn)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end   (region-end))
           (code  (buffer-substring-no-properties start end))
           (messages (funcall fn code)))
      (request openai-completion-api
        :type "POST"
        :headers openai-headers
        :data (openai-completion-request-create-body messages)
        :parser 'json-read
        :success (cl-function (lambda
                                (&key data &allow-other-keys)
                                (when data (openai-completion-log-to-buffer data))))
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (message "Got error: %S" error-thrown)))
        :complete (lambda (&rest _) (message "Finished!"))
        :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                       (418 . (lambda (&rest _) (message "Got 418."))))))))

(defun openai-completion-say-hello ()
  (interactive)
  (openai-completion-send-region (lambda (code)
                                   `((assistant . "I am very brief and don't answer additional text.")
                                     (user      . ,(format "Say hello to %s" code))))))

(defun openai-completion-explain-code (&optional language)
  (interactive)
  (let ((language (or language "")))
    (openai-completion-send-region (lambda (code)
                                     `((assistant . "I am very brief.")
                                       (user      . ,(format "Please explain the following %s code:\n%s" language code)))))))

;;; openai-playground.el ends here
