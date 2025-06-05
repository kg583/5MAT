; Read program from STDIN
; Make sure to use ~ before aesthetic newlines
(defvar *program* (do ((program "" (format nil "~a~&~a" program line)) (line "" (read-line t nil))) ((null line) program)))

; Initial tape is nil
(setq tape nil)

; Loop forever (until error)
(loop
 (format t
         "~{~v,'^~:*~a~}" ; Print only the portion before the first ETB (there are Lispier ways to do this...)
         (setq tape (coerce
                     (format nil *program* tape) ; Write the new tape
                     'list))))                   ; Coerce to a list since FORMAT can't loop over strings
