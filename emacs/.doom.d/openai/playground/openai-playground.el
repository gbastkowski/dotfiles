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

;;
;; Public variables
;;

(defconst openai-playground-completion-api   "https://api.openai.com/v1/chat/completions")
(defconst openai-playground-project          "proj_iowmSorSlumW84As4VhYe9mq")
(defconst openai-playground-org              "org-VujiJPSpiJilwgH1K9z5pnnL")
(defconst openai-playground-completion-model "gpt-4o-mini")
(defconst openai-playground-headers          `(("Content-Type"        . "application/json")
                                               ("Authorization"       . ,(concat "Bearer " (password-store-get "private/openai/emacs-api-key")))
                                               ("OpenAI-Organization" . ,openai-org)
                                               ("OpenAI-Project"      . ,openai-project)))

;;
;; Private functions
;;

(defun openai-playground--completion-request-create-body (messages &optional model)
  (let ((model (or model openai-playground-completion-model))
        (messages (mapcar (lambda (pair)
                            `(("role"    . ,(symbol-name (car pair)))
                              ("content" . ,(cdr pair))))
                          messages)))
    (json-encode `(("model"    . ,model)
                   ("messages" . ,messages)))))

(defun openai-playground--completion-response-extract-messages (data)
  (let* ((choices (alist-get 'choices data))
        (first-choice (aref choices 0))
        (message (alist-get 'message first-choice))
        (content (alist-get 'content message)))
    (format "%s\n" content)))

(defun openai-playground--completion-log-to-buffer (text &optional pop)
  (with-current-buffer (get-buffer-create "*openai completion log*")
    (goto-char (point-max))
    (insert text)))

(defun openai-playground--completion-show-in-posframe (text)
  (posframe-show "*openai-playground-output*"
                 :string "text"
                 :poshandler posframe-poshandler-frame-bottom-right-corner
                 :border-width 1
                 :border-color "gray"
                 :initialize (lambda () (markdown-mode))))

(setq openai-playground-output-function
      (lambda (text)
        (posframe-show "*openai-playground-output*"
                       :string "text"
                       :poshandler posframe-poshandler-frame-bottom-right-corner
                       :border-width 1
                       :border-color "gray"
                       :initialize (lambda () (markdown-mode)))))

;;
;; Public functions
;;

(defun openai-playground-completion-send-region (fn)
  (when (use-region-p)
    (let* ((start    (region-beginning))
           (end      (region-end))
           (code     (buffer-substring-no-properties start end))
           (messages (funcall fn code)))
      (request openai-playground-completion-api
        :type "POST"
        :headers openai-playground-headers
        :data (openai-playground--completion-request-create-body messages)
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (when data
                      ;; (funcall openai-playground-output-function (openai-playground--completion-response-extract-messages data))
                      (posframe-show "*openai-playground-output*"
                       :string "text"
                       :poshandler posframe-poshandler-frame-bottom-right-corner
                       :border-width 1
                       :border-color "gray"
                       :initialize (lambda () (markdown-mode)))
                      )))

        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (message "Got error: %S" error-thrown)))

        :complete (lambda (&rest _) (message "Finished!"))

        :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                       (418 . (lambda (&rest _) (message "Got 418."))))))))

;;
;; Public interactive functions
;;

(defun openai-playground-completion-say-hello ()
  (interactive)
  (openai-playground-completion-send-region (lambda (code)
                                               `((assistant . "I am very brief and don't answer additional text.")
                                                 (user      . ,(format "Say hello to %s" code))))))

(defun openai-playground-completion-explain-code (&optional language)
  (interactive)
  (let ((language (or language "")))
    (openai-playground-completion-send-region
     (lambda (code)
       `((assistant . "I am very brief. User knows the context and the programming language in which the code is written.")
         (user      . ,(format "Please explain the following %s code:\n%s" language code))
         (user      . "Start with a short paragraph which describes what the code does."))))))


(posframe-delete-all)

;;; openai-playground.el ends here
