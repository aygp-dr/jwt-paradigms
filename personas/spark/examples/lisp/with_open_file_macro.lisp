;; A simplified implementation of the 'with-open-file' macro
(defmacro with-open-file ((var filename &rest options) &body body)
  `(let ((,var (open ,filename ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (close ,var)))))

;; Usage
(with-open-file (stream "data.txt" :direction :input)
  (read-line stream)
  (process-data stream))

;; Expands to code that handles file opening and ensures proper cleanup
;; even if an error occurs during processing
