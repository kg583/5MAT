; Whether to print output + data
(defvar *debug* t)

; Maximum number of loops; set to nil to disable
(defvar *max-loops* 100)

(defvar *program* (car *args*))

(defun decode (string)
  (with-input-from-string (stream string)
    (coerce
      (loop while (listen stream) collect
        (case (setq char (read-char stream))
          (#\\
            (case (read-char stream)
              (#\x (int-char (+ (* (digit-char-p (read-char stream) 16) 16)
                                   (digit-char-p (read-char stream) 16))))
              (#\t #\tab)
              (#\n #\nl)
              (#\f #\ff)
              (#\r #\cr)))
          (#\↡ #\ff)
          (t char)))
      'string)))

(defun output(string)
  (if *debug*
      (print string)
      (format t (format nil "~~{~~v,'~|^~~:*~~a~~}") string)))

(defun driver()
  (setq tape (coerce (format nil *program* tape) 'list)))

(setq tape nil)
(setq *program* (decode *program*))

(if *max-loops*
    (dotimes' *max-loops* (output (driver)))
    (loop (output (driver))))
