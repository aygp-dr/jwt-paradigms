;; Using Clojure's homoiconicity for a query DSL
(query users
  (where [age > 21])
  (and [status = :active])
  (order-by [:created-at :desc])
  (limit 10))
