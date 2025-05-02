;; The Y combinator
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;; Using Y to define factorial
(define factorial
  (Y (lambda (f)
       (lambda (n)
         (if (zero? n)
             1
             (* n (f (- n 1))))))))

;; Test
(factorial 5) ; => 120
