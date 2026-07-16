;;; tools/claude-code/config.el -*- lexical-binding: t; -*-

(use-package! websocket)

(use-package! eat
  :commands (eat eat-other-window eat-project))

(use-package! claude-code-ide
  :init
  (setq claude-code-ide-terminal-backend 'eat)
  :config
  ;; Emacs code-intelligence tools are provided by mcp-emacs, so the
  ;; built-in tools server is intentionally left disabled here.
  (map! :leader
        :desc "Claude Code" "e" #'claude-code-ide-menu))
