(defconst gunnar-org-packages '(org, org-caldav))

(defun gunnar-org/init-osm ()
  (use-package osm
    :bind (("C-c m h" . osm-home)
           ("C-c m s" . osm-search)
           ("C-c m v" . osm-server)
           ("C-c m t" . osm-goto)
           ("C-c m x" . osm-gpx-show)
           ("C-c m j" . osm-bookmark-jump))

  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information

  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol))))

(defun gunnar-org/init-org-caldav ()
  (use-package org-caldav
    :init
    (setq org-caldav-url "https://cloud.bastkowski.name/remote.php/dav/calendars/gunnar")
    (setq org-caldav-calendar-id "personal")
    (setq org-caldav-inbox "~/org/calendars/gunnar.org")
    (setq org-caldav-files '("~/org/calendars/gunnar.org"))))


(defun gunnar-org/post-init-org ()
  (setq org-bullets-bullet-list '("x" "◆" "▴" "▸"))
  (setq org-directory "~/org/")
  (setq org-agenda-include-diary t)
  (setq org-default-notes-file (concat org-directory "inbox-yesomeo.org"))
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (awk . t)
                                   (ditaa . t)
                                   (dot . t)
                                   (java . t)
                                   (dot . t)
                                   (plantuml . t)
                                   (ruby . t)
                                   ))
  (defun gunnar/daily-note ()
    (concat org-directory (format-time-string "/%Y/%B_%-e.org")))

  (setq org-capture-templates '(
                                ("i" "Inbox"          entry (file "")
                                 "* TODO %^{Description}%?%i\n  %U\n" :immediate-finish t)
                                ("l" "Bookmarks"      entry (file "")
                                 "** %(org-cliplink-capture)%?\n  %U\n" :immediate-finish t)
                                ("s" "Step"           entry (clock)
                                 "* %^{Description}%?%i\n   %U\n")
                                ;; ("l" "Bookmarks"      entry (file+headline (lambda () (gunnar/daily-note)) "Bookmarks")
                                ;;  "** %(org-cliplink-capture)%?\n" :unnarrowed t)
                                ("x" "org-protocol"   entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO Review %c\n%U\n%i\n Added: %U\n" :immediate-finish)))

  (setq org-refile-targets '(("~/org/projects.org"     :maxlevel . 3)
                             ("~/org/someday.org"      :maxlevel . 3)
                             ("~/org/bookmarks.org"    :maxlevel . 5)
                             ("~/org/tickler.org"      :level    . 1)))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)")))

  (setq org-agenda-custom-commands
        '(("a" "Agenda and next items" ((agenda)
                                        (tags-todo "CATEGORY=\"inbox\""
                                              ((org-agenda-sorting-strategy '(priority-down))
                                               (org-agenda-overriding-header "Inbox")
                                               (org-agenda-todo-keyword-format "")))
                                        (todo "NEXT"
                                              ((org-agenda-sorting-strategy '(priority-down))
                                               (org-agenda-overriding-header "Next Actions")
                                               (org-agenda-todo-keyword-format "")))
                                        (todo "WAITING"
                                              ((org-agenda-sorting-strategy '(priority-down))
                                               (org-agenda-overriding-header "Waiting For")
                                               (org-agenda-todo-keyword-format ""))))
           ((org-agenda-span 'day)))

          ("b" "Mobimeo Agenda"        ((agenda)
                                        (tags-todo "@mobimeo+TODO=\"NEXT\""
                                                   ((org-agenda-sorting-strategy '(priority-down))
                                                    (org-agenda-overriding-header "Next Actions")
                                                    (org-agenda-todo-keyword-format "")))
                                        (tags-todo "@mobimeo+TODO=\"WAITING\""
                                                   ((org-agenda-sorting-strategy '(priority-down))
                                                    (org-agenda-overriding-header "Waiting For")
                                                    (org-agenda-todo-keyword-format "")))
                                        (tags-todo "@mobimeo+TODO=\"TODO\""
                                                   ((org-agenda-overriding-header "TODO Items")
                                                    (org-agenda-todo-keyword-format ""))))
           ((org-agenda-span 'day)))

          ("c" "Inbox" tags-todo "CATEGORY=\"inbox\""
           ((org-agenda-overriding-header "Inbox")))
          ))

  (setq org-agenda-prefix-format
        '((agenda . " %i %-20:c%?-12t% s")
          (todo . " %i %-20:c")
          (tags . " %i %-20:c")
          (search . " %i %-20:c")))

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo) (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry (or (outline-next-heading) (goto-char (point-max))))))

  (defun org-current-is-todo () (string= "TODO" (org-get-todo-state)))

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq diary-file     (concat org-directory "diary"))
  (setq timeclock-file (concat org-directory "timeclock"))

  (setq org-mobile-directory "~/org/")

  (setq plantuml-jar-path
        (file-name-concat (file-name-parent-directory (file-name-directory (file-chase-links "/opt/homebrew/bin/plantuml")))
                          "libexec"
                          "plantuml.jar"))
  (setq org-plantuml-jar-path
        (file-name-concat (file-name-parent-directory (file-name-directory (file-chase-links "/opt/homebrew/bin/plantuml")))
                          "libexec"
                          "plantuml.jar"))

  (defun org-insert-src-block (src-code-type)
    "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
    (interactive (let ((src-code-types
                        '(
                          "gunnar-test"
                          "C" "C++" "R" "clojure" "css" "ditaa" "dot" "emacs-lisp" "gnuplot" "haskell" "http"
                          "java" "js" "latex" "lisp" "org" "plantuml" "python" "ruby"
                          "sass" "scala" "sh" "sql" "sqlite")))
                   (list (ido-completing-read "Source code type: " src-code-types))))
    (progn (newline-and-indent)
           (insert (format "#+BEGIN_SRC %s\n" src-code-type))
           (newline-and-indent)
           (insert "#+END_SRC\n")
           (previous-line 2)
           (org-edit-src-code)))

  (add-hook 'org-mode-hook '(lambda ()
                              ;; turn on flyspell-mode by default
                              (flyspell-mode 1)
                              ;; C-TAB for expanding
                              (local-set-key (kbd "C-<tab>")
                                             'yas-expand-from-trigger-key)
                              ;; keybinding for editing source code blocks
                              (local-set-key (kbd "C-c s e")
                                             'org-edit-src-code)
                              ;; keybinding for inserting code blocks
                              (local-set-key (kbd "C-c s i")
                                             'org-insert-src-block)
                              ))
)
