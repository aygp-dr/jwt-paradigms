;; Define a protocol for expressions
(defprotocol Expr
  (eval-expr [this])
  (pretty-print [this]))

;; Implement base expression types
(defrecord Literal [value]
  Expr
  (eval-expr [_] value)
  (pretty-print [_] (str value)))

(defrecord Addition [left right]
  Expr
  (eval-expr [_] (+ (eval-expr left) (eval-expr right)))
  (pretty-print [_] (str "(" (pretty-print left) " + " (pretty-print right) ")")))

;; Later, extend with new operations
(defprotocol ExprOptimization
  (optimize [this]))

;; Extend existing types with new operations
(extend-protocol ExprOptimization
  Literal
  (optimize [this] this)
  
  Addition
  (optimize [this]
    (let [left (optimize (:left this))
          right (optimize (:right this))]
      (if (and (instance? Literal left) (instance? Literal right))
        (Literal. (+ (:value left) (:value right)))
        (Addition. left right)))))

;; Later, add new expression types
(defrecord Multiplication [left right]
  Expr
  (eval-expr [_] (* (eval-expr left) (eval-expr right)))
  (pretty-print [_] (str "(" (pretty-print left) " * " (pretty-print right) ")"))
  
  ExprOptimization
  (optimize [this]
    (let [left (optimize (:left this))
          right (optimize (:right this))]
      (if (and (instance? Literal left) (instance? Literal right))
        (Literal. (* (:value left) (:value right)))
        (Multiplication. left right)))))
