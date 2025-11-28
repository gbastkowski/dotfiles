;;; tools/codex/config.el -*- lexical-binding: t; -*-

(use-package! codex-cli
  :config
  (map! :leader
        :prefix ("d" . "Codex CLI")
        :desc "Codex CLI toggle" "t" #'codex-cli-toggle
        :desc "Start Codex CLI" "s" #'codex-cli-start
        :desc "Stop Codex CLI" "q" #'codex-cli-stop
        :desc "Stop all Codex sessions" "Q" #'codex-cli-stop-all
        :desc "Send prompt to Codex" "p" #'codex-cli-send-prompt
        :desc "Send region to Codex" "r" #'codex-cli-send-region
        :desc "Send file to Codex" "f" #'codex-cli-send-file
        :desc "Toggle Codex show-all" "a" #'codex-cli-toggle-all
        :desc "Show-all next page" "n" #'codex-cli-toggle-all-next-page
        :desc "Show-all previous page" "b" #'codex-cli-toggle-all-prev-page)
  (setq codex-cli-executable "codex"
        codex-cli-terminal-backend 'vterm
        codex-cli-side 'right
        codex-cli-width 90))
