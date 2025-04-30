;; Define a macro for a simplified 'unless' construct
(defmacro unless (condition &rest body)
  `(if (not ,condition)
       (progn ,@body)))

;; Usage
(unless (> x 10)
  (print "x is not greater than 10")
  (decrement x))

;; Expands at compile-time to:
(if (not (> x 10))
    (progn
      (print "x is not greater than 10")
      (decrement x)))
