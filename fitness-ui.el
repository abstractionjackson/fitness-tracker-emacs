;;; fitness-ui.el --- Fitness tracking commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025 abstractionjackson

;;; Commentary:

;; Provides commands for logging exercises and generating plots.

(require 'fitness-data)
(require 'fitness-plot)

(defun fitness-capture-exercise ()
  "Prompt the user to log an exercise in the fitness tracking database."
  (interactive)
  (let* ((date (read-string "Date (YYYY-MM-DD): " (format-time-string "%Y-%m-%d")))
         (distinct-movements (get-distinct-movements))
         (movement (completing-read "Movement: " distinct-movements nil nil))
         ;; Get weight with immediate feedback
         (weight-str (read-string "Weight (lbs): "))
         (weight (string-to-number weight-str))
         ;; Get sets with immediate feedback
         (sets-str (read-string "Sets: "))
         (sets (string-to-number sets-str))
         ;; Get reps with immediate feedback
         (reps-str (read-string "Reps per set: "))
         (reps (string-to-number reps-str)))

    ;; Debug output before save
    (message "About to save: Date=%S Movement=%S Weight=%S Sets=%S Reps=%S"
             date movement weight sets reps)

    ;; Verify we have valid data
    (if (and date
             (not (string-empty-p movement))
             (> weight 0)
             (> sets 0)
             (> reps 0))
        (progn
          (save-exercise date movement weight sets reps)
          (message "Exercise logged successfully!"))
      (message "Invalid input: Please check all fields are filled correctly"))))

(defun fitness-plot-exercise ()
  "Generate and display a plot of the fitness data."
  (interactive)
  (if (fitness-plot-generate)  ; Call generate and check return value
      (fitness-plot-display)   ; Display if generation successful
    (message "Failed to generate plot. Check if you have exercise data logged.")))

(provide 'fitness-ui)
;;; fitness-ui.el ends here
