;;; tools/codex/config.el -*- lexical-binding: t; -*-

(use-package! codex-cli
  :config
  (map! :leader
        :desc "Codex CLI toggle" "d t" #'codex-cli-toggle)
  ;; (("C-c c t" . codex-cli-toggle)
  ;;        ("C-c c s" . codex-cli-start)
  ;;        ("C-c c q" . codex-cli-stop)
  ;;        ("C-c c Q" . codex-cli-stop-all)
  ;;        ("C-c c p" . codex-cli-send-prompt)
  ;;        ("C-c c r" . codex-cli-send-region)
  ;;        ("C-c c f" . codex-cli-send-file)
  ;;        ;; Show-all layout + paging
  ;;        ("C-c c a" . codex-cli-toggle-all)
  ;;        ("C-c c n" . codex-cli-toggle-all-next-page)
  ;;        ("C-c c b" . codex-cli-toggle-all-prev-page))
  (setq codex-cli-executable "codex"
        codex-cli-terminal-backend 'vterm
        codex-cli-side 'right
        codex-cli-width 90))
