; Whether to print data + output
(defvar *debug* t)

; Maximum number of loops; set to nil to disable
(defvar *max-loops* 100)

(defvar *program* (car *args*))

(defun decode (string)
  (with-input-from-string (stream string)
    (coerce
      (loop while (listen stream) collect
        (let ((c (read-char stream)))
          (case c
            (#\\
              (case (read-char stream)
                (#\x (code-char (+ (* (digit-char-p (read-char stream) 16) 16)
                                      (digit-char-p (read-char stream) 16))))
                (#\a #\bell)
                (#\b #\backspace)
                (#\t #\tab)
                (#\n #\nl)
                (#\v #\vt)
                (#\f #\ff)
                (#\r #\cr)))
            (#\↡ #\ff)
            (t c))))
      'string)))

(defvar tape nil)
(defun output(string)
  (if *debug*
      (print string)
      (format t "~{~a~}"
        (subseq string (or (position #\ff string :from-end t) 0)))))

(defun driver()
  (setq tape (coerce (format nil *program* tape) 'list)))

(setq *program* (decode *program*))

(if *max-loops*
    (dotimes' *max-loops* (output (driver)))
    (loop (output (driver))))
