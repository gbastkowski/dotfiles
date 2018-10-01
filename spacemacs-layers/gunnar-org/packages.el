(defconst gunnar-org-packages '(org))

(defun gunnar-org/post-init-org ()
  (setq org-bullets-bullet-list '("●" "◆" "▴" "▸"))
  (setq org-directory "~/org/")
  (setq org-agenda-include-diary t)
  (setq org-default-notes-file (concat org-directory "gtd.org"))
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (awk . t)
                                   (ditaa . t)
                                   (dot . t)
                                   (java . t)
                                   (dot . t)
                                   (plantuml . t)
                                   (ruby . t)
                                   (scala . t)))
  (setq org-plantuml-jar-path
        (expand-file-name "/usr/local/Cellar/plantuml/1.2018.10/libexec/plantuml.jar"))
  (setq org-capture-templates '(
                                ("i" "Inbox"          entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %^{Brief Description} %^g\n %?%i\n Added: %U\n")
                                ("t" "todo"           entry (file+headline org-default-notes-file "Tasks")
                                 "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n  %a\n")
                                ("r" "Reading List"   entry (file+headline org-default-notes-file "Reading List")
                                 "")
                                ("j" "Journal"        entry (file+datetree (concat org-directory "bookmarks.org"))
                                 "")
                                ("k" "Knowledge"      entry (file          (concat org-directory "notes.org"))
                                 "")
                                ("x" "org-protocol"   entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO Review %c\n%U\n%i\n Added: %U\n" :immediate-finish)))

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
)
