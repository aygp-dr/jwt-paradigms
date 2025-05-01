-- The Maybe monad
-- Representing computations that might fail
data Maybe a = Nothing | Just a
  deriving (Show)

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> x = fmap f x

instance Monad Maybe where
    return x = Just x
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x
    
-- The List monad
-- Representing non-deterministic computations
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    
-- The State monad
-- Representing stateful computations
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f m = State $ \s -> 
        let (a, s') = runState m s
        in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    sf <*> sx = State $ \s ->
        let (f, s') = runState sf s
            (x, s'') = runState sx s'
        in (f x, s'')

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= f = State $ \s ->
        let (a, s') = runState m s
        in runState (f a) s'

-- Example: Computing with Maybe
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

computation :: Int -> Int -> Int -> Maybe Int
computation x y z = do
    a <- safeDiv x y
    b <- safeDiv z 2
    return (a + b)

-- Tests
main :: IO ()
main = do
    putStrLn "Maybe Monad Tests:"
    print $ safeDiv 10 2            -- Just 5
    print $ safeDiv 10 0            -- Nothing
    print $ computation 10 2 4      -- Just 7
    print $ computation 10 0 4      -- Nothing
    print $ computation 10 2 0      -- Just 5
    
    putStrLn "\nList Monad Tests:"
    print $ [1,2,3] >>= \x -> [x, x*2]  -- [1,2,2,4,3,6]
    
    putStrLn "\nState Monad Tests:"
    let incState = State $ \s -> (s, s+1)
    print $ runState (incState >>= \x -> return (x*2)) 1  -- (2,2)
