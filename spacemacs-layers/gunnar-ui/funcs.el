(defun gunnar-set-frame-height (lines)
  "Sets the height of the current (selected) frame to the given value"
  (interactive "p")
  (if (> 20 lines)
      (set-frame-height (selected-frame) lines)))
(spacemacs/set-leader-keys "Fh" 'spacemacs/set-frame-height)

(defun gunnar-set-frame-width (columns)
  "Sets the width of the current (selected) frame to the given value"
  (interactive "p")
  (if (> 20 columns)
      (set-frame-width (selected-frame) columns)))
(spacemacs/set-leader-keys "Fw" 'spacemacs/set-frame-width)

(defun gunnar-set-frame-size (columns lines)
  "Sets the height of the current (selected) frame to the given value"
  (interactive "cColumns:" "cLines:")
  (progn
    (message "Columns: " columns " Lines: " lines)))
