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

; Define the echo stream for I/O and the tape
(defvar out (make-string-output-stream))
(defvar echo (make-echo-stream *terminal-io* out))

; Initial tape is nil
(defvar tape nil)

; Output following the last form feed
(defun output(string)
  (princ (subseq string (or (position #\ff string :from-end t) 0))))

(setq *program* (decode *program*))

; Loop forever (until error)
(loop
  (format echo *program*                               ; Write the new tape to the echo stream
    (coerce tape 'list))                               ; Coerce to a list since FORMAT can't loop over strings
  (output (setq tape (get-output-stream-string out)))) ; Fetch and output the new tape
