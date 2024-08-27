;;; update-table.el --- Manage job application table

;;; Commentary:
;; This package provides functions to manage a job application table in org-mode.

;;; Code:

(require 'org)
(require 'cl-lib)

(defvar ut-table-columns '("Company" "Status" "Updated" "Opening" "Deadline")
  "List of table column names.")

(defvar ut-table-statuses '("*OPEN*" "*CLOSED*" "*APPLIED*" "*MISSED*")
  "List of possible statuses for the job applications.")

(defun ut-table--get-column-index (column-name)
  "Get the index of COLUMN-NAME in the table.
COLUMN-NAME: string
Return: integer or nil"
  (cl-position column-name ut-table-columns :test 'string=))

(defun ut-table--get-current-row ()
  "Get the current row as a list of cells.
Return: list of strings"
  (save-excursion
    (let ((row ()))
      (beginning-of-line)
      (org-table-next-field)
      (dotimes (_ (length ut-table-columns))
        (push (org-table-get-field) row)
        (org-table-next-field))
      (nreverse row))))

(defun ut-table--update-row (row)
  "Update the current row with values from ROW.
ROW: list of strings
Return: nil"
  (org-table-goto-column 1) 
  (dotimes (i (length row))
    (org-table-goto-column (+ i 1))
    (org-table-blank-field)
    (insert (nth i row))))

(defun ut-table--trim-string (str)
  "Remove leading and trailing whitespace from STR."
  (if (string-match "\\`[ \t\n\r]*\\(.*?\\)[ \t\n\r]*\\'" str)
      (match-string 1 str)
    str))

(defun ut-table-toggle-status ()
  "Toggle the status of the current row and update the date.
Return: nil"
  (interactive)
  (let ((row-start (point))    ;; Save the current point
        (column (org-table-current-column)))  ;; Save the current column
    (let* ((row (ut-table--get-current-row))
           (status-index (ut-table--get-column-index "Status"))
           (date-index (ut-table--get-column-index "Updated"))
           (current-status (ut-table--trim-string (nth status-index row)))
           (status-pos (cl-position current-status ut-table-statuses :test 'string=))
           (next-status (if status-pos
                            (nth (mod (+ status-pos 1) (length ut-table-statuses)) ut-table-statuses)
                          (car ut-table-statuses)))
           (current-date (format-time-string "<%Y-%m-%d>")))
      (message "Current status: %s, Status position: %s, Next status: %s"
               current-status status-pos next-status)
      (setf (nth status-index row) (format "%s" next-status))
      (setf (nth date-index row) current-date)
      (ut-table--update-row row)
      (org-table-align)
      ;; Restore cursor position
      (goto-char row-start)  ;; Restore the saved point
      (org-table-goto-column column))))

(defun ut-table-update-date ()
  "Update the 'Updated' column with the current date.
Return: nil"
  (interactive)
  (let ((row-start (point))    ;; Save the current point
        (column (org-table-current-column)))  ;; Save the current column
    (let* ((row (ut-table--get-current-row))
           (date-index (ut-table--get-column-index "Updated"))
           (current-date (format-time-string "<%Y-%m-%d>")))
      (setf (nth date-index row) current-date)
      (ut-table--update-row row)
      (org-table-align)
      ;; Restore cursor position
      (goto-char row-start)  ;; Restore the saved point
      (org-table-goto-column column))))

(defun ut-table-sort-by-column (column-name)
  "Sort the table by COLUMN-NAME.
COLUMN-NAME: string
Return: nil"
  (interactive (list (completing-read "Column: " ut-table-columns)))
  (let ((row-start (point))    ;; Save the current point
        (column (org-table-current-column)))  ;; Save the current column
    (let ((column-index (ut-table--get-column-index column-name)))
      (org-table-sort-lines nil ?N column-index)
      (org-table-align)
      ;; Restore cursor position
      (goto-char row-start)  ;; Restore the saved point
      (org-table-goto-column column))))

(global-set-key (kbd "C-c c s") 'ut-table-toggle-status)
(global-set-key (kbd "C-c c u") 'ut-table-update-date)
(global-set-key (kbd "C-c c o") 'ut-table-sort-by-column)

(provide 'ut-table-manager)
;;; ut-table-manager.el ends here
