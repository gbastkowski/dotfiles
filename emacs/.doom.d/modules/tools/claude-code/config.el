;;; tools/claude-code/config.el -*- lexical-binding: t; -*-

(use-package! claude-code
  :bind-keymap  ("C-c c" . claude-code-command-map)
  :config       (claude-code-mode))

(after! claude-code
  (add-to-list 'display-buffer-alist
               '("^\\*claude"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.3))))


;; Cross-platform notification functions
(defun gunnar/macos-notify (title message)
  "Display macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format
                      "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                      message
                      title)))

(defun gunnar/linux-notify (title message)
  "Display Linux notification using notify-send."
  (call-process "notify-send" nil nil nil title message))

(defun gunnar/claude-notify (title message)
  "Display cross-platform notification for Claude Code."
  (cond ((eq system-type 'darwin)                                           (gunnar/macos-notify title message))
        ((and (eq system-type 'gnu/linux) (executable-find "notify-send"))  (gunnar/linux-notify title message))
        (t                                                                  (message "%s: %s" title message))))
