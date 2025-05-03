#lang racket

(provide (all-from-out racket)
         forward right left penup pendown)

(define turtle-x 0)
(define turtle-y 0)
(define turtle-angle 0)
(define turtle-pen-down #t)

(define (forward distance)
  (define new-x (+ turtle-x (* distance (cos (degrees->radians turtle-angle)))))
  (define new-y (+ turtle-y (* distance (sin (degrees->radians turtle-angle)))))
  (when turtle-pen-down
    (draw-line turtle-x turtle-y new-x new-y))
  (set! turtle-x new-x)
  (set! turtle-y new-y))

(define (right angle)
  (set! turtle-angle (modulo (- turtle-angle angle) 360)))

(define (left angle)
  (set! turtle-angle (modulo (+ turtle-angle angle) 360)))

(define (penup)
  (set! turtle-pen-down #f))

(define (pendown)
  (set! turtle-pen-down #t))
