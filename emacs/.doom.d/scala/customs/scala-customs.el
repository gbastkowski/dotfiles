;;; scala-customs.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Gunnar Bastkowski
;;
;; Author: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Maintainer: Gunnar Bastkowski <gunnar@bastkowski.name>
;; Created: August 26, 2025
;; Modified: August 26, 2025
;; Version: 0.0.1
;; Keywords:  lisp local
;; Homepage: https://github.com/gbastkowski/scala-customs
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun scala-organize-imports ()
  "Organize Scala imports like IntelliJ IDEA.
Groups imports in the following order:
1. Java/Scala standard library imports (java.*, javax.*, scala.*)
2. Third-party library imports
3. Project imports (usually same package/organization)
Within each group, imports are sorted alphabetically."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((imports '())
          (import-start nil)
          (import-end nil))
      
      ;; Collect all import statements
      (while (re-search-forward "^\\s-*import\\s-+\\([^{}\n]+\\)" nil t)
        (let ((import-line (match-string 0))
              (import-content (match-string 1)))
          (push (cons import-content (string-trim import-line)) imports)
          (when (null import-start)
            (setq import-start (line-beginning-position)))
          (setq import-end (line-end-position))
          (delete-region (line-beginning-position) (1+ (line-end-position)))))
      
      (when imports
        ;; Sort and group imports
        (let* ((sorted-imports (sort imports (lambda (a b) (string< (car a) (car b)))))
               (java-stdlib '())
               (scala-stdlib '())
               (third-party '())
               (project '()))
          
          ;; Categorize imports
          (dolist (import sorted-imports)
            (let ((content (car import)))
              (cond
               ((string-match-p "^java\\." content)
                (push import java-stdlib))
               ((string-match-p "^javax\\." content)
                (push import java-stdlib))
               ((string-match-p "^scala\\." content)
                (push import scala-stdlib))
               ((string-match-p "^akka\\." content)
                (push import third-party))
               ((string-match-p "^play\\." content)
                (push import third-party))
               ((string-match-p "^cats\\." content)
                (push import third-party))
               ((string-match-p "^io\\." content)
                (push import third-party))
               ((string-match-p "^org\\." content)
                (push import third-party))
               ((string-match-p "^com\\." content)
                (push import third-party))
               (t
                (push import project)))))
          
          ;; Insert organized imports
          (goto-char import-start)
          (let ((groups (list (reverse java-stdlib)
                             (reverse scala-stdlib)
                             (reverse third-party)
                             (reverse project)))
                (first-group t))
            
            (dolist (group groups)
              (when group
                (unless first-group
                  (insert "\n"))
                (dolist (import group)
                  (insert (cdr import) "\n"))
                (setq first-group nil)))))))
    
    (message "Imports organized")))

(defun scala-remove-unused-imports ()
  "Remove unused import statements (basic implementation).
This is a simplified version - for full functionality use LSP or Metals."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((imports-to-check '()))
      
      ;; Collect all imports
      (while (re-search-forward "^\\s-*import\\s-+\\([^{}\n]+\\)" nil t)
        (let ((import-content (match-string 1))
              (import-line-start (line-beginning-position))
              (import-line-end (1+ (line-end-position))))
          (push (list import-content import-line-start import-line-end) imports-to-check)))
      
      ;; Check if imports are used (simple text search)
      (dolist (import-info imports-to-check)
        (let* ((import-content (first import-info))
               (line-start (second import-info))
               (line-end (third import-info))
               (import-name (if (string-match "\\([^.]+\\)$" import-content)
                               (match-string 1 import-content)
                             import-content)))
          
          ;; Simple check: if the imported name doesn't appear elsewhere, mark for deletion
          (goto-char (point-min))
          (let ((found-usage nil))
            (while (and (not found-usage) 
                       (re-search-forward (regexp-quote import-name) nil t))
              (unless (and (>= (point) line-start) (< (point) line-end))
                (setq found-usage t)))
            
            (unless found-usage
              (delete-region line-start line-end)
              (message "Removed unused import: %s" import-content))))))))

(defun scala-add-import (import-statement)
  "Add an import statement in the correct location according to IntelliJ ordering."
  (interactive "sImport statement: ")
  (unless (string-match "^import\\s-+" import-statement)
    (setq import-statement (concat "import " import-statement)))
  
  (save-excursion
    (goto-char (point-min))
    
    ;; Find the last import or package declaration
    (let ((insert-point nil))
      (cond
       ;; Look for existing imports
       ((re-search-forward "^\\s-*import\\s-+" nil t)
        (beginning-of-line)
        (while (and (looking-at "^\\s-*import\\s-+") (not (eobp)))
          (forward-line 1))
        (setq insert-point (point)))
       
       ;; Look for package declaration
       ((progn (goto-char (point-min))
               (re-search-forward "^\\s-*package\\s-+" nil t))
        (end-of-line)
        (forward-line 1)
        (insert "\n")
        (setq insert-point (point)))
       
       ;; Insert at the beginning
       (t
        (goto-char (point-min))
        (setq insert-point (point))))
      
      (when insert-point
        (goto-char insert-point)
        (insert import-statement "\n")
        (scala-organize-imports)))))

(provide 'scala-customs)

;;; scala-customs.el ends here
