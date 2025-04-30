#lang s-exp "imperative.rkt"

(define x 0)

(while (< x 10)
  (displayln x)
  (inc! x))
