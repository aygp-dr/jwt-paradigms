-- Logical AND corresponds to product types
-- Proof that A AND B implies A
fst :: (a, b) -> a
fst (a, _) = a

-- Logical OR corresponds to sum types
-- Proof that A implies A OR B
left :: a -> Either a b
left a = Left a

-- Logical implication corresponds to function types
-- Modus ponens: If (A implies B) and A, then B
apply :: (a -> b) -> a -> b
apply f a = f a
