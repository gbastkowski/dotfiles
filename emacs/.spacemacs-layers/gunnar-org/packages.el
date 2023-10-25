(defconst gunnar-org-packages '(org))

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

(defun gunnar-org/post-init-org ()
  (setq org-bullets-bullet-list '("x" "◆" "▴" "▸"))
  (setq org-directory "~/org/")
  (setq org-agenda-include-diary t)
  (setq org-default-notes-file (concat org-directory "inbox.org"))
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
                                ("i" "Inbox"          entry (file "~/org/inbox.org")
                                 "* TODO %^{Description}%?%i\n   %U\n")
                                ("T" "Tickler"        entry (file "~/org/tickler.org")
                                 "* %^{Description}%?%i\n   %U\n")
                                ("t" "todo"           entry (file+headline org-default-notes-file "Tasks")
                                 "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n  %a\n")
                                ("r" "Reading List"   entry (file+headline org-default-notes-file "Reading List")
                                 "")
                                ("j" "Journal"        entry (file+datetree (concat org-directory "bookmarks.org"))
                                 "")
                                ("k" "Knowledge"      entry (file          (concat org-directory "notes.org"))
                                 "")
                                ("l" "Bookmarks"      entry (file+headline (lambda () (gunnar/daily-note)) "Bookmarks")
                                 "** %(org-cliplink-capture)%?\n" :unnarrowed t)
                                ("x" "org-protocol"   entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO Review %c\n%U\n%i\n Added: %U\n" :immediate-finish)))
  (setq org-refile-targets '(("~/org/gtd.org"     :maxlevel . 3)
                             ("~/org/someday.org" :level    . 1)
                             ("~/org/tickler.org" :level    . 1)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq diary-file     (concat org-directory "diary"))
  (setq timeclock-file (concat org-directory "timeclock"))

  (setq org-feed-alist '(
                         ("Slashdot"
                          "http://rss.slashdot.org/Slashdot/slashdot"
                          (concat org-directory "feeds.org")
                          "Slashdot Entries")))

  (setq org-mobile-directory "~/org/")

  (setq plantuml-jar-path
        (file-name-concat (file-name-parent-directory (file-name-directory (file-chase-links "/opt/homebrew/bin/plantuml")))
                          "libexec"
                          "plantuml.jar"))
  (setq org-plantuml-jar-path
        (file-name-concat (file-name-parent-directory (file-name-directory (file-chase-links "/opt/homebrew/bin/plantuml")))
                          "libexec"
                          "plantuml.jar"))

  (file-name-concat (file-name-parent-directory (file-name-directory (file-chase-links "/opt/homebrew/bin/plantuml")))
                    "libexec"
                    "plantuml.jar")

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
                                             'yas/expand-from-trigger-key)
                              ;; keybinding for editing source code blocks
                              (local-set-key (kbd "C-c s e")
                                             'org-edit-src-code)
                              ;; keybinding for inserting code blocks
                              (local-set-key (kbd "C-c s i")
                                             'org-insert-src-block)
                              ))
)
