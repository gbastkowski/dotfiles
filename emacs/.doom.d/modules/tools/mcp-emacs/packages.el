;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
;; plz: HTTP/SSE backend for the opencode client (soft dependency of mcp-emacs).
(package! plz)
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :files ("elisp/*.el"))
  ;; Doom's `:pin' takes a commit hash only -- it abbreviates the value with
  ;; `substring', so a tag name errors out (doom-packages.el).
  ;;
  ;; v1.12.3
  :pin "39bf3db1341896b5989b761523665d8418ea6f37")
