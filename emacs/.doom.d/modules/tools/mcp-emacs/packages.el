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
  ;; Merge of gate-tool-permissions-in-emacs, ahead of the 1.12.0 release:
  ;; running the permission gate locally before cutting the tag.
  :pin "3e7312bfe64daf9f60e2b0c700a81ea41c47cf33")
