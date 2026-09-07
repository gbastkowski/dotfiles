;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
;; plz: HTTP/SSE backend for the opencode client (soft dependency of mcp-emacs).
(package! plz)
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :branch "compose-prompts-in-a-buffer"
                              :files ("elisp/*.el"))
  ;; Trying out the prompt composition buffer before merging.  Doom's `:pin'
  ;; takes a commit hash only -- it abbreviates the value with `substring',
  ;; so a tag name errors out (doom-packages.el).
  :pin "821bf9c7c9ca22382e00a4b25c91b5fae1e6ccda")
