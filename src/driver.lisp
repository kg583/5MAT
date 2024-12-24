; Read program from STDIN (single line only)
(defvar *program* (read-line))

; Initial tape is nil
(setq tape nil)

; Loop forever (until error)
(loop
 (format t
         "~{~v,'^~:*~a~}" ; Print only the portion before the first ETB (there are Lispier ways to do this...)
         (setq data (coerce
                     (format nil *program* tape) ; Write the new tape
                     'list))))                   ; Coerce to a list since FORMAT can't loop over strings
