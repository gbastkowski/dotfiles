(defun gunnar/all-calendars-to-diary ()
  (interactive)
  (let ((google-calendar-url (password-store-get "mobimeo/calendar/url"))
        (ical-filename (make-temp-file "google-calendar.ics")))
    (progn
      (gunnar/some-calendar-to-diary (password-store-get "mobimeo/calendar/url"))
      (gunnar/some-calendar-to-diary (password-store-get "private/cloud.bastkowski.name/calendar-gunnar-url"))
      )))

(defun gunnar/some-calendar-to-diary (calendar-url)
  (let ((ical-filename (make-temp-file "calendar.ics")))
    (progn
      (url-copy-file calendar-url ical-filename t)
      (set-buffer (find-file-noselect ical-filename))
      (icalendar-import-buffer diary-file t)
      (kill-buffer)
      'ok)))

(setq org-caldav-calendars '((:calendar-id "work@whatever" :files ("~/org/work.org")
                                           :inbox "~/org/fromwork.org")
                             (:calendar-id "stuff@mystuff"
                                           :files ("~/org/sports.org" "~/org/play.org")
                                           :inbox "~/org/fromstuff.org")))

(defun gunnar/org-agenda-show-inbox (&optional arg) (interactive "P") (org-agenda arg "c"))
(defun gunnar/org-agenda-show-today (&optional arg) (interactive "P") (org-agenda arg "a"))
(defun gunnar/org-agenda-show-today-mobimeo (&optional arg) (interactive "P") (org-agenda arg "b"))
(defun gunnar/org-capture-inbox (&optional arg) (interactive "P") (org-capture arg "i"))

(spacemacs/set-leader-keys "Ga" 'gunnar/org-agenda-show-today)
(spacemacs/set-leader-keys "Gb" 'gunnar/org-agenda-show-today-mobimeo)
(spacemacs/set-leader-keys "Gc" 'gunnar/org-agenda-show-inbox)

(spacemacs/set-leader-keys "Gi" 'gunnar/org-capture-inbox)

(spacemacs/set-leader-keys "Gs" 'gunnar/all-calendars-to-diary)
