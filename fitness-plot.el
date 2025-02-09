;;; fitness-plot.el --- Visualization for Fitness Tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025 abstractionjackson

;;; Commentary

;; This file provides functions for generating fitness plots using an external Python script.
;; It requires the 'fitness-data' package and sets up paths for the package directory,
;; Python executable, and plot module.

(require 'fitness-data)

;; Path to the package directory
(defvar fitness-tracker-dir (file-name-directory (or load-file-name buffer-file-name)))

;; Path to Python virtual environment
(defvar fitness-tracker-venv-dir (expand-file-name ".venv" fitness-tracker-dir))
(defvar fitness-tracker-venv-bin (expand-file-name "bin" fitness-tracker-venv-dir))

;; Path to the python executable in venv
(defvar fitness-tracker-python-executable
  (expand-file-name
   (if (eq system-type 'windows-nt) "python.exe" "python")
   fitness-tracker-venv-bin))

;; Path to requirements.txt
(defvar fitness-tracker-requirements
  (expand-file-name "requirements.txt" fitness-tracker-dir))

;; Path to the plot module
(defvar fitness-tracker-plot-script
  (expand-file-name "plot.py" fitness-tracker-dir))

(defun fitness-plot-display ()
  "Display the fitness plot in a buffer."
  (let ((buffer (get-buffer-create "*Fitness Plot*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (file-exists-p fitness-tracker-plot-file)
            (progn
              (insert-image (create-image fitness-tracker-plot-file))
              (image-mode)
              (read-only-mode 1))
          (insert "No plot available. Try logging some exercises first."))))
    (switch-to-buffer buffer)))

(defun fitness-tracker-ensure-venv ()
  "Ensure Python virtual environment exists and has required packages."
  (unless (file-exists-p fitness-tracker-python-executable)
    (message "Setting up Python virtual environment...")
    (let ((default-directory fitness-tracker-dir))
      ;; Create virtual environment
      (call-process "python" nil nil t "-m" "venv" ".venv")

      ;; Install requirements
      (let ((pip (expand-file-name
                 (if (eq system-type 'windows-nt)
                     "Scripts/pip.exe"
                   "bin/pip")
                 fitness-tracker-venv-dir)))
        (call-process pip nil nil t
                     "install" "-r" fitness-tracker-requirements))
      (message "Python environment setup complete!"))))

(defun fitness-plot-generate ()
  "Generate the fitness plot.
Returns t if successful, nil otherwise."
  (fitness-tracker-ensure-venv)
  (= 0 (shell-command
        (format "%s %s"
               (shell-quote-argument fitness-tracker-python-executable)
               (shell-quote-argument fitness-tracker-plot-script)))))

(provide 'fitness-plot)
;;; fitness-plot.el ends here
