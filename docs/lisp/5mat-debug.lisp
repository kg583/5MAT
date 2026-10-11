; Whether to print entire tape each loop
(defvar *debug* t)

; Maximum number of loops; set to nil to disable
(defvar *max-loops* 100)

; Program to run (default is first command-line argument)
(defvar *program* (car *args*))

; Initial tape contents (default is empty)
(defvar tape nil)

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

(defvar out (make-string-output-stream))
(defvar echo (make-echo-stream *terminal-io* out))

(defun output(string)
  (if *debug*
      (print string)
      (princ (subseq string (or (position #\ff string :from-end t) 0)))))

(defun driver()
  (format echo *program* (coerce tape 'list))
  (setq tape (get-output-stream-string out)))

(setq *program* (decode *program*))

(if *max-loops*
    (dotimes' *max-loops* (output (driver)))
    (loop (output (driver))))
