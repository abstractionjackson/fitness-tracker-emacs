;;; fitness-data.el --- Data Handling for Fitness Tracker -*- lexical-binding: t; -*-

(defvar fitness-data-file "~/fitness.org"
  "Path to the Org file storing fitness data.")

(defun fitness-data-save-entry (date movement weight sets reps)
  "Save an exercise entry to the fitness tracking table."
  (with-current-buffer (find-file-noselect fitness-data-file)
    (goto-char (point-min))
    (unless (search-forward "#+NAME: Fitness-Log" nil t)
      (goto-char (point-max))
      (insert "\n#+NAME: Fitness-Log\n| Date | Movement | Weight | Sets | Reps |\n|------|----------|--------|------|------|\n"))
    (forward-line)
    (while (org-at-table-p) (forward-line 1))
    (insert (format "| %s | %s | %s | %s | %s |\n" date movement weight sets reps))
    (org-table-align)
    (save-buffer)))

(provide 'fitness-data)
