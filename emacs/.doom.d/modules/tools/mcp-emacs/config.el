;;; tools/mcp-emacs/config.el -*- lexical-binding: t; -*-

;; MCP server that runs inside this Emacs session and speaks the Model
;; Context Protocol over HTTP, so AI agents reach the live buffers,
;; windows, and Org state directly (no emacsclient round-trip per call).
;; Point an MCP client at http://localhost:8765/mcp.

(use-package! mcp-emacs-server
  :defer t
  :init
  (add-hook 'emacs-startup-hook #'mcp-emacs-server-ensure))

;; Terminal runner that launches the Claude CLI inside Emacs (eat backend),
;; one primary session per project, reaching editor tools through the MCP
;; server above. Intended to replace claude-code-ide; kept alongside it for
;; now under a distinct SPC E prefix so both can be exercised.
(use-package! mcp-emacs-run
  :defer t
  :commands (mcp-emacs-run
             mcp-emacs-run-start
             mcp-emacs-run-continue
             mcp-emacs-run-resume
             mcp-emacs-run-list
             mcp-emacs-run-switch
             mcp-emacs-run-kill
             mcp-emacs-run-toggle)
  :config
  ;; Doom's +popup catch-all (^\*) would otherwise capture the runner buffer
  ;; into a bottom popup, overriding the runner's own directional window.
  (when (fboundp 'set-popup-rule!)
    (set-popup-rule! "^\\*claude:" :ignore t))
  :init
  (map! :leader
        (:prefix ("E" . "Claude runner")
         :desc "Start / switch to session"  "e" #'mcp-emacs-run
         :desc "Start session hidden"       "E" #'mcp-emacs-run-start
         :desc "Continue last conversation" "c" #'mcp-emacs-run-continue
         :desc "Resume a conversation"      "r" #'mcp-emacs-run-resume
         :desc "List live sessions"         "l" #'mcp-emacs-run-list
         :desc "Switch to a session"        "s" #'mcp-emacs-run-switch
         :desc "Kill this project's session" "k" #'mcp-emacs-run-kill
         :desc "Toggle runner window"       "t" #'mcp-emacs-run-toggle)))
