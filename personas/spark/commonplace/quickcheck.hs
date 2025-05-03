import System.Random (StdGen, mkStdGen, random, randomR)

-- Simplified version of QuickCheck's generator concept
newtype Gen a = Gen { unGen :: StdGen -> Int -> a }

instance Functor Gen where
    fmap f (Gen g) = Gen (\r n -> f (g r n))

instance Applicative Gen where
    pure x = Gen (\_ _ -> x)
    Gen f <*> Gen x = Gen (\r n ->
        let (r1, r2) = split r
            n' = n `div` 2
        in (f r1 n') (x r2 n'))

instance Monad Gen where
    return = pure
    Gen m >>= f = Gen (\r n ->
        let (r1, r2) = split r
            n' = n `div` 2
            a = m r1 n'
        in unGen (f a) r2 n')

-- Utility functions
sized :: (Int -> Gen a) -> Gen a
sized f = Gen $ \r n -> unGen (f n) r n

resize :: Int -> Gen a -> Gen a
resize m (Gen g) = Gen $ \r _ -> g r m

oneof :: [Gen a] -> Gen a
oneof gs = chooseInt (0, length gs - 1) >>= \i -> gs !! i

chooseInt :: (Int, Int) -> Gen Int
chooseInt (lo, hi) = Gen $ \r _ -> fst $ randomR (lo, hi) r

generate :: Gen a -> IO a
generate (Gen g) = return $ g (mkStdGen 42) 30

-- Simple implementation of split for StdGen
split :: StdGen -> (StdGen, StdGen)
split s = let (a, s') = random s
              (b, s'') = random s'
          in (mkStdGen a, mkStdGen b)

-- Generator for recursive structures (e.g., trees)
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

-- The elegant recursion happens here
genTree :: Gen a -> Gen (Tree a)
genTree genA = sized $ \n ->
    if n <= 1
    then fmap Leaf genA
    else do
        -- Recursively generate smaller trees
        let genSmaller = resize (n `div` 2) (genTree genA)
        oneof [
            fmap Leaf genA,
            liftA2 Branch genSmaller genSmaller
            ]

-- Simple generator for integers
genInt :: Gen Int
genInt = chooseInt (-100, 100)

-- Example usage
main :: IO ()
main = do
    putStrLn "Generating random trees:"
    tree1 <- generate (genTree genInt)
    putStrLn $ "Tree 1: " ++ show tree1
    tree2 <- generate (genTree genInt)
    putStrLn $ "Tree 2: " ++ show tree2
