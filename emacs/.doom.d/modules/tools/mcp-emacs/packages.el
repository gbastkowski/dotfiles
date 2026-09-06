;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
;; plz: HTTP/SSE backend for the opencode client (soft dependency of mcp-emacs).
(package! plz)
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :branch "main"
                              :files ("elisp/*.el"))
  ;; v1.10.1.  Doom's `:pin' takes a commit hash only -- it abbreviates the
  ;; value with `substring', so a tag name errors out (doom-packages.el).
  :pin "4a6cd2a6ad53aebacd1049e354d0f93abb6bd6fd")
