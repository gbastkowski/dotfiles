(defun gumacs/mac-p () (eq system-type 'darwin))
(defun gumacs/linux-p () (eq system-type 'gnu/linux))
(defun gumacs/mswindows-p () (eq system-type 'windows-nt))

(defun gumacs/window-system-mac-p ()
  ;; ns is returned instead of mac on Emacs 25+
  (memq (window-system) '(mac ns)))

(package-install 'bind-map)

(defun gumacs/init-bind-map ()
  (require 'bind-map)
  (bind-map gumacs-default-map
    :prefix-cmd gumacs-cmds
    :keys ("M-m")
    :evil-keys ("SPC")
    :override-minor-modes t
    :override-mode-name gumacs-leader-override-mode))

(defun gumacs/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under the leader key.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(spacemacs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key gumacs-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defun gumacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (save-some-buffers nil t)
  (kill-emacs))

(defun gumacs/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (kill-emacs))

(defun gumacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))
