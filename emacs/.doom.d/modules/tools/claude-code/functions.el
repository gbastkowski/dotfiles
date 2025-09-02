;;; tools/claude-code/functions.el -*- lexical-binding: t; -*-

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