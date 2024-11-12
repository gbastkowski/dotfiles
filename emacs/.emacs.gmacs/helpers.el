;;; helpers.el -*- lexical-binding: t; -*-
(defun gmacs/load-config (file)
  "Load a FILE relative to the user's Emacs directory."
  (load-file (expand-file-name file user-emacs-directory)))
