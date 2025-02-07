;;; fitness-data.el --- Data Handling for Fitness Tracker -*- lexical-binding: t; -*-

(defvar fitness-db-file "~/fitness-tracker/fitness.sqlite3"
  "Path to the SQLite database storing fitness data.")

(defun init-db ()
  "Initialize the database connection and create tables if they don't exist."
  (let ((db (sqlite-open (expand-file-name fitness-db-file))))
    (sqlite-execute db "CREATE TABLE IF NOT EXISTS exercises
                       (id INTEGER PRIMARY KEY AUTOINCREMENT,
                        date DATE NOT NULL,
                        movement TEXT NOT NULL,
                        weight NUMERIC NOT NULL,
                        sets INTEGER NOT NULL,
                        reps INTEGER NOT NULL)")
    db))

(defun save-exercise (date movement weight reps sets)
  "Save a new exercise entry to the database."
  (let ((db (init-db)))
    (sqlite-execute db "INSERT INTO exercises (date, movement, weight, reps, sets)
                       VALUES (?, ?, ?, ?, ?)"
                   (list date movement weight reps sets))
    (sqlite-close db)))

(defun get-distinct-movements ()
  "Retrieve a list of distinct movements from the database."
  (let ((db (init-db)))
    (let ((movements (sqlite-select db "SELECT DISTINCT movement FROM exercises")))
      (sqlite-close db)
      (mapcar 'car movements))))

(init-db)
(provide 'fitness-data)
