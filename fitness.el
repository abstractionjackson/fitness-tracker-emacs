;;; fitness.el --- Simple Fitness Tracker for Emacs -*- lexical-binding: t; -*-

(require 'fitness-data)
(require 'fitness-plot)

(defun fitness-get-existing-movements ()
  "Retrieve a list of existing movements from the fitness data table."
  (with-current-buffer (find-file-noselect fitness-data-file)
    (goto-char (point-min))
    (if (search-forward "#+NAME: Fitness-Log" nil t)
        (let (movements)
          (while (re-search-forward "| \([0-9-]+\) | \([^|]+\) |" nil t)
            (add-to-list 'movements (match-string 2)))
          movements)
      '())))

(defun fitness-capture-exercise ()
  "Prompt the user to log an exercise in the fitness tracking table."
  (interactive)
  (let* ((date (read-string "Date (YYYY-MM-DD): " (format-time-string "%Y-%m-%d")))
         (existing-movements (fitness-get-existing-movements))
         (movement (completing-read "Movement: " existing-movements))
         (weight (read-string "Weight (lbs): "))
         (sets (read-string "Sets: "))
         (reps (read-string "Reps per set: ")))
    (fitness-data-save-entry date movement weight sets reps)
    (message "Exercise logged successfully!")))

(defun fitness-plot-exercise ()
  "Generate a plot of the fitness data using Python."
  (interactive)
  (fitness-plot-generate)
  (message "Plot generated! Check 'lifting_progress.png'."))

(provide 'fitness)
