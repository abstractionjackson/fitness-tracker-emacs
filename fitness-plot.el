;;; fitness-plot.el --- Visualization for Fitness Tracker -*- lexical-binding: t; -*-

(require 'fitness-data)

;; Path to the package directory
(setq package-directory "~/fitness-tracker")

;; Path to the python executable (inside the package dir, in .venv)
(setq python-executable (concat package-directory "/.venv/bin/python"))

;; Path to the plot module
(setq plot-module (concat package-directory "/plot.py"))

(defun fitness-plot-generate ()
  "Call an external Python script to generate the exercise plot."
  ;; the python script is at plot.py in the fitness-tracker directory
  (shell-command (concat python-executable " " plot-module)))

(provide 'fitness-plot)
