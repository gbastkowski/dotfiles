;;; early-init.el -*- lexical-binding: t; -*-

;; Remove clutter from UI
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'early-init)
;;; early-init.el ends here

(load-file (expand-file-name "helpers.el" user-emacs-directory))
