;;; fitness-data.el --- Data Handling for Fitness Tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025 abstractionjackson

;;; Commentary:

;; This file provides functions for handling fitness data, including
;; initializing the database, saving exercise entries, and retrieving
;; distinct movements.

;;; Code:

(require 'sqlite3)

(defvar fitness-tracker-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where the fitness-tracker package is installed.")

(defvar fitness-db-file
  (expand-file-name "fitness.sqlite3" fitness-tracker-dir)
  "Path to the SQLite database storing fitness data.")

(defun init-db ()
  "Initialize the database connection and create tables if they don't exist."
  ;; Ensure directory exists
  (make-directory (file-name-directory fitness-db-file) t)
  (let ((db (sqlite-open fitness-db-file)))
    (sqlite-execute db "CREATE TABLE IF NOT EXISTS exercises
                       (id INTEGER PRIMARY KEY AUTOINCREMENT,
                        date DATE NOT NULL,
                        movement TEXT NOT NULL,
                        weight NUMERIC NOT NULL,
                        sets INTEGER NOT NULL,
                        reps INTEGER NOT NULL)")
    ;; Add a test to verify table creation
    (let ((table-check (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table' AND name='exercises'")))
      (unless table-check
        (error "Failed to create exercises table")))
    (sqlite-close db)
    t)) ; Return true if successful

(defun save-exercise (date movement weight reps sets)
  "Save a new exercise entry to the database."
  (let ((db (sqlite-open fitness-db-file)))
    (sqlite-execute db "INSERT INTO exercises (date, movement, weight, reps, sets)
                       VALUES (?, ?, ?, ?, ?)"
                   (list date movement weight reps sets))
    (sqlite-close db)))

(defun get-distinct-movements ()
  "Retrieve a list of distinct movements from the database."
  (let ((db (sqlite-open fitness-db-file)))
    (let ((movements (sqlite-select db "SELECT DISTINCT movement FROM exercises")))
      (sqlite-close db)
      (or (mapcar 'car movements) '()))))

;; Remove the init-db call here since we're handling it in fitness-tracker.el
(provide 'fitness-data)
;;; fitness-data.el ends here
