(defclass prioritized-method-combination (standard-method-combination)
  ((priority :initarg :priority :accessor priority)))

(defmethod compute-effective-method ((combination prioritized-method-combination)
                                     generic-function
                                     methods)
  (let ((sorted-methods (sort (copy-list methods)
                              #'>
                              :key #'priority)))
    ;; Call the highest priority method first
    `(call-method ,(first sorted-methods)
                  ,(rest sorted-methods))))
