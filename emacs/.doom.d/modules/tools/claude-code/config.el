;;; tools/claude-code/config.el -*- lexical-binding: t; -*-

(use-package! claude-code
  :bind-keymap  ("C-c c" . claude-code-command-map)
  :config       (claude-code-mode))

(after! claude-code
  (set-popup-rule! "^\\*claude" :side 'right :width 0.3 :quit t :select t))

