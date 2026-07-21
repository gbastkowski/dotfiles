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
             mcp-emacs-run-toggle
             mcp-emacs-explain-selection-in-current-session
             mcp-emacs-run-send-return
             mcp-emacs-run-send-1
             mcp-emacs-run-send-2
             mcp-emacs-run-send-3
             mcp-emacs-run-send-shift-tab
             mcp-emacs-run-send-up
             mcp-emacs-run-send-down)
  :config
  ;; Doom's +popup catch-all (^\*) would otherwise capture the runner buffer
  ;; into a bottom popup, overriding the runner's own directional window.
  (when (fboundp 'set-popup-rule!)
    (set-popup-rule! "^\\*claude:" :ignore t)
    ;; Same for the popup output window (e.g. *mcp-emacs:explain*): let the
    ;; package place it in its own directional split rather than a +popup.
    (set-popup-rule! "^\\*mcp-emacs:" :ignore t))
  :init
  (map! :leader
        (:prefix ("E" . "Claude runner")
         :desc "Start / switch to session"  "e" #'mcp-emacs-run
         :desc "Start session hidden"       "E" #'mcp-emacs-run-start
         :desc "Continue last conversation" "c" #'mcp-emacs-run-continue
         :desc "Resume a conversation"      "r" #'mcp-emacs-run-resume
         :desc "List live sessions"         "l" #'mcp-emacs-run-list
         :desc "Switch to a session"        "s" #'mcp-emacs-run-switch
         :desc "Kill this project's session" "K" #'mcp-emacs-run-kill
         :desc "Toggle runner window"       "t" #'mcp-emacs-run-toggle
         (:prefix ("a" . "AI actions")
          :desc "Explain selection"         "e" #'mcp-emacs-explain-selection-in-current-session)
         (:prefix ("k" . "Send keystroke")
          :desc "Return"                    "RET" #'mcp-emacs-run-send-return
          :desc "Choose 1"                  "1"   #'mcp-emacs-run-send-1
          :desc "Choose 2"                  "2"   #'mcp-emacs-run-send-2
          :desc "Choose 3"                  "3"   #'mcp-emacs-run-send-3
          :desc "Cycle mode (shift-tab)"    "<tab>" #'mcp-emacs-run-send-shift-tab
          :desc "Up"                        "p"   #'mcp-emacs-run-send-up
          :desc "Down"                      "n"   #'mcp-emacs-run-send-down))))
