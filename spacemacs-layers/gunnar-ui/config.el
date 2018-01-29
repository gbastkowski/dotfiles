
(set-frame-height (selected-frame) 70)
(set-frame-width (selected-frame) 200)

;; Fix MacOS key bindings
(setq mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-right-command-modifier 'meta
      mac-option-modifier nil
      ns-function-modifier 'super)

(setq frame-title-format '(buffer-file-name "%f" "Emacs: %b"))

(setq ns-auto-hide-menu-bar nil)
