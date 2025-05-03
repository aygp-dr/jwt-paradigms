;; Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

;; Successor function
(define succ 
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

;; Addition
(define add
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f) ((n f) x)))))))

;; Multiplication
(define mult
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (m (n f))))))

;; Church to integer conversion (for demonstration)
(define church->int
  (lambda (church)
    ((church (lambda (n) (+ n 1))) 0)))

;; Test
(church->int (((add two) three))) ; => 5
(church->int ((mult two) three))  ; => 6
