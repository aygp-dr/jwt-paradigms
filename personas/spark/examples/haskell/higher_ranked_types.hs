{-# LANGUAGE RankNTypes #-}

-- A function that applies a higher-order function to two different arguments
applyTwice :: (forall a. a -> a) -> (b -> b, c -> c)
applyTwice f = (f, f)

-- Usage
duplicate :: String -> String
duplicate s = s ++ s

main = do
  let (f, g) = applyTwice duplicate
  print (f "hello")  -- "hellohello"
  print (g 42)       -- Type error: g expects String, got Integer
