;;; fitness-plot.el --- Visualization for Fitness Tracker -*- lexical-binding: t; -*-

(require 'fitness-data)

(defun fitness-plot-generate ()
  "Call an external Python script to generate the exercise plot."
  (let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
    (shell-command "python3 plot.py")))

(provide 'fitness-plot)
