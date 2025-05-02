;; In Lisp, this expression:
(+ 1 (* 2 3))

;; Is represented as this data structure:
'(+ 1 (* 2 3))

;; Which can be manipulated like any other list:
(car '(+ 1 (* 2 3)))  ; => +
(cadr '(+ 1 (* 2 3))) ; => 1
(caddr '(+ 1 (* 2 3))) ; => (* 2 3)

;; And can be constructed and evaluated:
(eval (list '+ 1 (list '* 2 3))) ; => 7
