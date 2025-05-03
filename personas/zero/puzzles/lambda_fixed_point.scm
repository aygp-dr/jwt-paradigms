;; ================================================================
;; Lambda Calculus Fixed Point Puzzle (by Zero)
;; ================================================================
;;
;; CHALLENGE: Implement the Y combinator in pure lambda calculus
;; without using Scheme's built-in recursion
;;
;; Background: The Y combinator is a fixed-point combinator that allows 
;; recursion to be defined in the lambda calculus, which has no built-in
;; recursion mechanism.
;;
;; Your task: Complete the implementation of my-y-combinator below
;; so it can be used to implement the factorial function without
;; explicit recursion.
;;
;; RULES:
;; 1. You may only use lambda expressions and function application
;; 2. NO CHEATING with Scheme's built-in recursion or Y combinator
;; 3. Your solution should work with pure lambda calculus semantics

;; λf.(λx.f(x x))(λx.f(x x))

;; Incomplete Y combinator - needs fixing
(define (incomplete-y-combinator f)
  ;; PUZZLE: Replace this with the correct implementation
  (lambda (n) (if (zero? n) 1 (* n ((f f) (- n 1))))))

;; A factorial function that takes a "self" parameter
;; (This is not recursive itself - recursion comes from the Y combinator)
(define (almost-factorial self)
  (lambda (n)
    (if (zero? n)
        1
        (* n (self (- n 1))))))

;; This should work if your Y combinator is correct
;; (factorial 5) => 120
(define factorial (my-y-combinator almost-factorial))

;; ================================================================
;; Zero's Commentary:
;; ================================================================
;;
;; "The beauty of the Y combinator is that it reveals the fundamental
;; nature of recursion as a fixed point operation. By encoding infinite
;; regress into a static form, it demonstrates that systems with only
;; simple rules of substitution can express the full power of computation.
;;
;; When most programmers hear 'recursion', they think of a language
;; feature. But lambda calculus shows it's actually a mathematical
;; inevitability - it emerges naturally from the system. This is why
;; I maintain that functional programming isn't just a style, it's
;; the fundamental substrate of computation itself.
;;
;; Anyone who solves this puzzle truly understands that FP isn't just
;; 'programming without side effects' - it's an elegant formalization
;; of computation itself. The object-oriented crowd are merely playing 
;; with shadows on the cave wall."
;;
;; - Zero  
;;
;; P.S. Once you solve this, try implementing call/cc in lambda calculus!
;; That's where things get *really* interesting...