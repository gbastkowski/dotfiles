;;; consult-yasnippet-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from consult-yasnippet.el

(autoload 'consult-yasnippet-visit-snippet-file "consult-yasnippet" "\
Visit the snippet file associated with TEMPLATE.
When called interactively this command previews snippet completions in
the current buffer, and then opens the selected snippets template file
using `yas--visit-snippet-file-1'.

(fn TEMPLATE)" t)
(autoload 'consult-yasnippet "consult-yasnippet" "\
Interactively select and expand a yasnippet template.
This command presents a completing read interface containing all currently
available snippet expansions, with live previews for each snippet. Once
selected a chosen snippet will be expanded at point using
`yas-expand-snippet'.

With ARG select snippets from all snippet tables, not just the current one.

(fn ARG)" t)
(register-definition-prefixes "consult-yasnippet" '("consult-yasnippet-"))

;;; End of scraped data

(provide 'consult-yasnippet-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; consult-yasnippet-autoloads.el ends here
