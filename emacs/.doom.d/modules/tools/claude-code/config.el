;;; tools/claude-code/config.el -*- lexical-binding: t; -*-

(use-package! claude-code
  :bind-keymap  ("C-c c" . claude-code-command-map)
  :config       (claude-code-mode))

(after! claude-code
  (add-to-list 'display-buffer-alist
               '("^\\*claude"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.3))))

