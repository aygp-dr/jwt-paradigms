#lang s-exp "turtle.rkt"

(define (square size)
  (forward size)
  (right 90)
  (forward size)
  (right 90)
  (forward size)
  (right 90)
  (forward size))

(square 100)
(right 45)
(square 70)
