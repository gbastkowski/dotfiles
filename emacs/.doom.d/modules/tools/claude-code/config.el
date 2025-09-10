;;; tools/claude-code/config.el -*- lexical-binding: t; -*-

(use-package! websocket)

(use-package! monet
  :config (monet-mode))

(use-package! claude-code
  :config (claude-code-mode))

(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(map! :leader
      :desc "Claude Code" "e" claude-code-command-map)
