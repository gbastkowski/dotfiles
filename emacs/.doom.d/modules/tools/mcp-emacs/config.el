;;; tools/mcp-emacs/config.el -*- lexical-binding: t; -*-

;; MCP server that runs inside this Emacs session and speaks the Model
;; Context Protocol over HTTP, so AI agents reach the live buffers,
;; windows, and Org state directly (no emacsclient round-trip per call).
;; Point an MCP client at http://localhost:8765/mcp.

(use-package! mcp-emacs-server
  :defer t
  :init
  (add-hook 'emacs-startup-hook #'mcp-emacs-server-ensure))

;; Native opencode HTTP/SSE client.
;; The server runs as an on-demand launchd agent (see the opencode home-manager module)
;; so its sessions survive Emacs restarts; `opencode-client-serve' kickstarts that agent.
;; The basic-auth password is read from pass, matching how the launchd agent gets it.
(use-package! opencode-client
  :defer t
  :commands (opencode-client-connect
             opencode-client-serve
             opencode-client-create-session
             opencode-client-switch-session
             opencode-client-list-sessions
             opencode-client-delete-session
             opencode-client-send-prompt
             opencode-client-interrupt)
  :init
  (setq opencode-client-launchd-label "org.nix-community.home.opencode-serve"
        opencode-client-password-command "pass show private/opencode/server-password"))

;; Terminal runner: the Claude CLI in an eat buffer, one session per project.  
(use-package! mcp-emacs-run
  :defer t
  :commands (mcp-emacs-run-new
             mcp-emacs-run-start
             mcp-emacs-run-continue
             mcp-emacs-run-resume
             mcp-emacs-run-list
             mcp-emacs-run-switch
             mcp-emacs-run-kill
             mcp-emacs-run-quit
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
        ;; SPC A carries its label here.  A second `map!' that re-declares the
        ;; prefix *with* a description builds a fresh keymap and drops whatever
        ;; the earlier block bound, so only one block may name it.
        (:prefix ("A" . "AI agent")
         (:prefix ("t" . "terminal (fallback)")
          :desc "Start new session"          "e" #'mcp-emacs-run-new
          :desc "Start session hidden"       "E" #'mcp-emacs-run-start
          :desc "Continue last conversation" "c" #'mcp-emacs-run-continue
          :desc "Resume a conversation"      "r" #'mcp-emacs-run-resume
          :desc "List live sessions"         "l" #'mcp-emacs-run-list
          :desc "Switch to a session"        "s" #'mcp-emacs-run-switch
          :desc "Kill this project's session" "K" #'mcp-emacs-run-kill
          :desc "Quit session (graceful)"    "q" #'mcp-emacs-run-quit
          :desc "Toggle runner window"       "w" #'mcp-emacs-run-toggle
          :desc "Explain selection"          "x" #'mcp-emacs-explain-selection-in-current-session
          :desc "Send Return"                "RET"   #'mcp-emacs-run-send-return
          :desc "Send 1"                     "1"     #'mcp-emacs-run-send-1
          :desc "Send 2"                     "2"     #'mcp-emacs-run-send-2
          :desc "Send 3"                     "3"     #'mcp-emacs-run-send-3
          :desc "Send shift-tab (cycle)"     "<tab>" #'mcp-emacs-run-send-shift-tab
          :desc "Send Up"                    "p"     #'mcp-emacs-run-send-up
          :desc "Send Down"                  "n"     #'mcp-emacs-run-send-down))))

;; Backend-agnostic agent runner: the first-class path.
;; `agent-backend-preference' picks opencode or Claude.
;; Both client modes derive from `agent-backend-mode', so C-c C-s/-i/-n/-q/-r work in the conversation buffer too.
(use-package! agent-backend
  :defer t
  :commands (agent-backend-start
             agent-backend-send-command
             agent-backend-add-note-command
             agent-backend-interrupt-command
             agent-backend-quit-command
             agent-backend-resume-command)
  :init
  ;; `auto' would start opencode whenever its launchd server answers.
  (setq agent-backend-preference 'claude)
  (map! :leader
        ;; Labelled by the mcp-emacs-run block above; naming it here too would
        ;; rebuild the map and drop the SPC A t sub-prefix.
        (:prefix "A"
         :desc "Start agent"         "a" #'agent-backend-start
         :desc "Send prompt"         "s" #'agent-backend-send-command
         :desc "Add note"            "n" #'agent-backend-add-note-command
         :desc "Interrupt turn"      "i" #'agent-backend-interrupt-command
         :desc "Resume conversation" "r" #'agent-backend-resume-command
         :desc "Quit conversation"   "q" #'agent-backend-quit-command))
  :config
  ;; Doom's +popup catch-all (^\*) would override each client's own
  ;; directional window.  `*claude-client*' has no colon, so the
  ;; "^\\*claude:" rule above does not cover it.
  (when (fboundp 'set-popup-rule!)
    (set-popup-rule! "^\\*claude-client\\*" :ignore t)
    (set-popup-rule! "^\\*opencode" :ignore t)))
