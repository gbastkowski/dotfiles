;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
;; plz: HTTP/SSE backend for the opencode client (soft dependency of mcp-emacs).
(package! plz)
;; Unpinned, like every other package here.  This one carried the config's
;; only `:pin' for a while, which went stale within days -- it is my own
;; package and releases automatically on every merge to main -- and since
;; `:pin' overrides `doom sync -u', each upgrade quietly restored the old
;; version instead of taking the new one.  Tracking main means every
;; `bin/system-upgrade.sh' run picks up whatever has landed.
;;
;; The cost is that an upgrade can pull an untested commit into the
;; editor.  To take a known-good version instead, put the `:pin' back with
;; `doom/bump-package-at-point' (point on this form) rather than copying a
;; hash by hand; Doom's `:pin' wants a commit id, not a tag name.
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :files ("elisp/*.el")))
