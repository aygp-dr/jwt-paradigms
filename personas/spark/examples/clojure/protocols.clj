(defprotocol Evaluable
  (eval [this]))

(defprotocol Printable
  (pretty-print [this]))

(defrecord Lit [value]
  Evaluable
  (eval [_] value)
  
  Printable
  (pretty-print [_] (str value)))

(defrecord Add [left right]
  Evaluable
  (eval [_] (+ (eval left) (eval right)))
  
  Printable
  (pretty-print [_] (str "(" (pretty-print left) " + " (pretty-print right) ")")))

;; Later, extend existing protocols to new types
(defrecord Div [numerator denominator]
  Evaluable
  (eval [_] (/ (eval numerator) (eval denominator)))
  
  Printable
  (pretty-print [_] (str "(" (pretty-print numerator) " / " (pretty-print denominator) ")")))

;; And extend existing types with new protocols
(defprotocol Optimizable
  (optimize [this]))

(extend-protocol Optimizable
  Lit
  (optimize [this] this)
  
  Add
  (optimize [this]
    (let [left' (optimize (:left this))
          right' (optimize (:right this))]
      (if (and (instance? Lit left') (instance? Lit right'))
        (Lit. (+ (:value left') (:value right')))
        (Add. left' right'))))
  
  Div
  (optimize [this]
    ;; Implementation for Div
    ))
