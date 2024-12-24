; Whether to print output + data
(defvar *debug* t)

; Maximum number of loops; set to nil to disable
(defvar *max-loops* 100)

(defvar *program* "YOUR PROGRAM HERE")
(setq data nil)

(defun output(string)
  (if *debug*
      (print string)
      (format t "~{~v,'^~:*~a~}" string)))

(defun driver()
  (setq data (coerce (format nil *program* data) 'list)))

(if *max-loops*
    (dotimes' *max-loops* (output (driver)))
    (loop (output (driver))))
