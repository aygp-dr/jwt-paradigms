;; Define a function that creates specialized multiplier functions
(defun make-multiplier (factor)
  (compile nil `(lambda (x) (* ,factor x))))

;; Create specialized multiplier functions
(defparameter *double* (make-multiplier 2))
(defparameter *triple* (make-multiplier 3))

;; Use the generated functions
(funcall *double* 5) ; => 10
(funcall *triple* 5) ; => 15
