; Read program from STDIN
(defvar *program* (read-line))

; Initial input is nil
(setq data nil)

; Loop forever (until error)
(loop
 (format t
         "~{~v,'^~:*~a~}" ; Print only the portion before the first ETB
         (setq data (coerce
                     (format nil *program* data) ; Format the data
                     'list))))                   ; Coerce to a list since FORMAT can't loop over strings
