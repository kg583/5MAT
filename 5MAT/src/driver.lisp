; Read program from *args*
(defvar *program* (car *args*))

; Decode escape sequences
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

; Output up to the last form feed
(defvar tape nil)
(defun output (string)
  (format t "~{~a~}"
    (subseq string (or (position #\ff string :from-end t) 0))))

; Initial tape is nil
(setq *program* (decode *program*))

; Loop forever (until error)
(loop
  (output (setq tape
    (coerce
      (format nil *program* tape) ; Write the new tape
      'list))))                   ; Coerce to a list since FORMAT can't loop over strings
