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

; Found at https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
; and decided that I like it
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(spacemacs/set-leader-keys "of" 'aj-toggle-fold)


;; (set-frame-height (selected-frame) 50)
;; (set-frame-width (selected-frame) 140)
