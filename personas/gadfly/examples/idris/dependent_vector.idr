-- A vector with statically checked length
Vector : (n : Nat) -> Type -> Type

-- Concatenation preserves length
concat : {a : Type} -> {m, n : Nat} -> Vector m a -> Vector n a -> Vector (m + n) a
