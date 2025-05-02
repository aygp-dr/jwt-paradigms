#lang racket

(provide (all-defined-out))

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))
      
(define-syntax-rule (inc! x)
  (set! x (+ x 1)))
  
(define-syntax-rule (dec! x)
  (set! x (- x 1)))
