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

(require 'markdown-mode)
(require 'posframe)

;;
;;; Public variables
;;

(defvar openai-playground-completion-api)
(defvar openai-playground-project)
(defvar openai-playground-org)
(defvar openai-playground-completion-model "gpt-4o-mini")

;;
;;; Private functions
;;

(defun openai-playground--request-headers ()
  `(("Content-Type"        . "application/json")
    ("Authorization"       . ,(concat "Bearer " (password-store-get "private/openai/emacs-api-key")))
    ("OpenAI-Organization" . ,openai-playground-org)
    ("OpenAI-Project"      . ,openai-playground-project)))

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
  (let ((posframe (posframe-show " *openai-playground-output*"
                                 :string text
                                 ;; :position (point)
                                 :poshandler #'posframe-poshandler-window-top-right-corner
                                 :x-pixel-offset 10
                                 :y-pixel-offset 10
                                 :max-width 80
                                 :border-width 2
                                 :border-color "green"
                                 :initialize (lambda () (markdown-mode)))))
    posframe))

(defun openai-playground--completion-show-in-sidewindow (text)
  (let ((buffer (get-buffer-create " *openai-playground-output*")))
    (with-current-buffer buffer
      (markdown-mode)
      (erase-buffer)
      (insert text))
    (display-buffer-in-side-window buffer
                                   '((side         . right)
                                     (window-width . 0.4)))))

(defun openai-playground--completion-show-in-window-right (text)
  (let ((buffer (get-buffer-create " *openai-playground-output*")))
    (with-current-buffer buffer
      (markdown-mode)
      (erase-buffer)
      (insert text))
    (display-buffer-in-direction buffer
                                   '((direction    . right)
                                     (window-width . 0.4)))))

(setq openai-playground-output-function #'openai-playground--completion-show-in-posframe)

;;
;;; Public functions
;;

(defun openai-playground-completion-send-region (fn)
  (when (use-region-p)
    (let* ((start    (region-beginning))
           (end      (region-end))
           (code     (buffer-substring-no-properties start end))
           (messages (funcall fn code)))
      (funcall openai-playground-output-function (concat "------------------------------------------\n"
                                                         "            **Please wait...**            \n"
                                                         "------------------------------------------\n"))
      (request openai-playground-completion-api
        :type "POST"
        :headers (openai-playground--request-headers)
        :data (openai-playground--completion-request-create-body messages)
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (when data
                      (funcall openai-playground-output-function (openai-playground--completion-response-extract-messages data)))))

        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (message "Got error: %S" error-thrown)))

        :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                       (418 . (lambda (&rest _) (message "Got 418."))))))))

;;
;;; Public interactive functions
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

;;; openai-playground.el ends here
