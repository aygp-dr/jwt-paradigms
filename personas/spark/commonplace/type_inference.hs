-- Expression: \f -> \x -> f (f x)

-- Step 1: Assign type variables
-- f :: a
-- x :: b
-- The expression has type: (a -> b) -> a -> b

-- Step 2: Generate constraints from application f x
-- Since f is applied to x, f must be a function
-- f :: b -> c (for some c)
-- This creates a constraint: a = (b -> c)

-- Step 3: Generate constraints from application f (f x)
-- f is applied to (f x), which has type c
-- This creates another constraint: a = (c -> d) (for some d)

-- Step 4: Unify constraints
-- a = (b -> c) and a = (c -> d)
-- Therefore: (b -> c) = (c -> d)
-- Which gives us: b = c and c = d

-- Step 5: Substitute
-- b = c = d
-- Therefore, the inferred type is: (b -> b) -> b -> b

-- In Haskell notation:
-- :t \f -> \x -> f (f x)
-- (\f -> \x -> f (f x)) :: (b -> b) -> b -> b

-- Actual runnable version for testing
twice :: (b -> b) -> b -> b
twice f x = f (f x)

-- Test with simple functions
main = do
  print $ twice (+1) 0        -- Should print 2
  print $ twice (*2) 3        -- Should print 12
  print $ twice reverse [1,2,3] -- Should print [1,2,3]
