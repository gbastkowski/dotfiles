;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Gunnar Bastkowski
;;
;; Author: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Maintainer: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Created: November 07, 2024
;; Modified: November 07, 2024

;; Setup straight package manager
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-pull-recipe-repositories)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load config.org as configuration file
(use-package org
  :ensure t)

;; Tangle config.org and load the resulting config.el file
(let ((config-file (expand-file-name "config.org" user-emacs-directory)))
  (when (file-exists-p config-file)
    (org-babel-load-file config-file)))

(provide 'init)
;;; init.el ends here
