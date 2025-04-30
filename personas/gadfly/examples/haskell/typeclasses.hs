-- Define a data type
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr

-- Define a typeclass for evaluation
class Evaluable a where
  eval :: a -> Int

-- Implement Evaluable for Expr
instance Evaluable Expr where
  eval (Lit n) = n
  eval (Add e1 e2) = eval e1 + eval e2
  eval (Mul e1 e2) = eval e1 * eval e2

-- Later, add a new operation without modifying Expr
class Printable a where
  prettyPrint :: a -> String

instance Printable Expr where
  prettyPrint (Lit n) = show n
  prettyPrint (Add e1 e2) = "(" ++ prettyPrint e1 ++ " + " ++ prettyPrint e2 ++ ")"
  prettyPrint (Mul e1 e2) = "(" ++ prettyPrint e1 ++ " * " ++ prettyPrint e2 ++ ")"

-- Even later, add a new data type that works with existing operations
data ExtendedExpr = Base Expr | Div ExtendedExpr ExtendedExpr

instance Evaluable ExtendedExpr where
  eval (Base e) = eval e
  eval (Div e1 e2) = eval e1 `div` eval e2

instance Printable ExtendedExpr where
  prettyPrint (Base e) = prettyPrint e
  prettyPrint (Div e1 e2) = "(" ++ prettyPrint e1 ++ " / " ++ prettyPrint e2 ++ ")"
