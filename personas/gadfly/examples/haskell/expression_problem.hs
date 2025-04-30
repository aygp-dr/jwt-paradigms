data Expr = Lit Int
          | Add Expr Expr
          | Mul Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

prettyPrint :: Expr -> String
prettyPrint (Lit n) = show n
prettyPrint (Add e1 e2) = "(" ++ prettyPrint e1 ++ " + " ++ prettyPrint e2 ++ ")"
prettyPrint (Mul e1 e2) = "(" ++ prettyPrint e1 ++ " * " ++ prettyPrint e2 ++ ")"
