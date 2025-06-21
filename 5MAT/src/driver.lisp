; Read program from STDIN
; Make sure to use ~ before aesthetic newlines
(defvar *program* (do ((program "" (format nil "~a~&~a" program line)) (line "" (read-line t nil)))
                      ((null line) (substitute #\FF #\↡ program))))

; Initial tape is nil
(setq tape nil)

; Loop forever (until error)
(defun output(string)
  (format t (format nil "~~{~~v,'~|^~~:*~~a~~}") string))

(loop
  (output (setq tape (coerce
                       (format nil *program* tape) ; Write the new tape
                       'list))))                   ; Coerce to a list since FORMAT can't loop over strings
