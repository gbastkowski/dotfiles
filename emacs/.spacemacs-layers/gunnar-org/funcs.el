
(defun google-calendar-to-diary ()
  (let ((ical-filename (make-temp-file "google-calendar.ics")))
    (progn
      (url-copy-file google-calendar-url ical-filename t)
      (set-buffer (find-file-noselect ical-filename))
      (icalendar-import-buffer diary-file t)
      (kill-buffer)
      'ok)))
