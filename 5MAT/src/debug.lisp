; Whether to print output + data
(defvar *debug* t)

; Maximum number of loops; set to nil to disable
(defvar *max-loops* 100)

(defvar *program* (substitute #\FF #\↡ "YOUR PROGRAM HERE"))
(setq tape nil)

(defun output(string)
  (if *debug*
      (print string)
      (format t (format nil "~~{~~v,'~|^~~:*~~a~~}") string)))

(defun driver()
  (setq tape (coerce (format nil *program* tape) 'list)))

(if *max-loops*
    (dotimes' *max-loops* (output (driver)))
    (loop (output (driver))))
