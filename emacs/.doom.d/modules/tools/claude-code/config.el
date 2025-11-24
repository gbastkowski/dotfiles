;;; tools/claude-code/config.el -*- lexical-binding: t; -*-

(use-package! websocket)

(use-package! monet
  :config (monet-mode))

(use-package! eat
  :commands (eat eat-other-window eat-project))

(use-package! claude-code
  :config 
  (claude-code-mode)
  (map! :leader
        :desc "Claude Code" "e" claude-code-command-map))

(use-package! claude-code-ide
  :config
  (claude-code-ide-emacs-tools-setup)
  (map! :leader
        :desc "Claude Code IDE" "E" #'claude-code-ide-menu))
