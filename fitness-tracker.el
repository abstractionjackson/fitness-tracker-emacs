;;; fitness-tracker.el --- A simple fitness tracking system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 abstractionjackson

;; Author: Jackson Galan <jacksongalan@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (sqlite3 "0.1"))
;; Keywords: tools
;; URL: https://github.com/abstractionjackson/fitness-tracker-emacs

;;; Commentary:

;; This package provides a simple fitness tracking system for Emacs.
;; It allows users to log their exercises and view progress through plots.

;;; Code:

(require 'fitness-data)
(require 'fitness-plot)
(require 'fitness-ui)

(defgroup fitness-tracker nil
  "A simple fitness tracking system."
  :group 'applications)

(defvar fitness-tracker-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where the fitness-tracker package is installed.")

;;;###autoload
(defun fitness-tracker-initialize ()
  "Initialize the fitness tracker."
  (interactive)
  ;; Ensure package directory exists and is writable
  (unless (file-writable-p fitness-tracker-dir)
    (error "Cannot write to fitness-tracker directory: %s" fitness-tracker-dir))
  (init-db)
  (fitness-tracker-ensure-venv))


;;;###autoload
(define-minor-mode fitness-tracker-mode
  "Toggle Fitness Tracker mode.
When enabled, provides commands for tracking fitness activities."
  :lighter " Fitness"
  :global t
  :group 'fitness-tracker
  (if fitness-tracker-mode
      (fitness-tracker-initialize)))

(provide 'fitness-tracker)
;;; fitness-tracker.el ends here
