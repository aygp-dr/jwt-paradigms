-- Functional approach
-- Easy to add operations, hard to add data variants
data Expr = Constant Double
          | Addition Expr Expr
          deriving (Show)

-- Instructions for our hypothetical virtual machine
data Instruction = PushConstant Double | Add
                 deriving (Show)

evaluate :: Expr -> Double
evaluate (Constant x) = x
evaluate (Addition e1 e2) = evaluate e1 + evaluate e2

prettyPrint :: Expr -> String
prettyPrint (Constant x) = show x
prettyPrint (Addition e1 e2) = "(" ++ prettyPrint e1 ++ " + " ++ prettyPrint e2 ++ ")"

-- Adding a new operation is easy
compile :: Expr -> [Instruction]
compile (Constant x) = [PushConstant x]
compile (Addition e1 e2) = compile e1 ++ compile e2 ++ [Add]

-- Example usage
main :: IO ()
main = do
    let expr = Addition (Constant 5) (Addition (Constant 3) (Constant 2))
    putStrLn $ "Expression: " ++ prettyPrint expr
    putStrLn $ "Evaluated: " ++ show (evaluate expr)
    putStrLn $ "Compiled: " ++ show (compile expr)

-- Adding a new data variant requires modifying all functions!
