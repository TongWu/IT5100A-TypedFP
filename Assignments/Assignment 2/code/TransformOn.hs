module TransformOn(TransformOn(..)) where

class TransformOn a b where
  -- | Applies a transformation (a -> a) on all occurrences of 'a' in a term of type 'b'
  transformOn :: (a -> a) -> b -> b

-- Examples
instance TransformOn Int Int where
  transformOn :: (Int -> Int) -> Int -> Int
  transformOn = id -- transformOn f x = f x; since x is just an Int, directly applying it to f suffices

instance (TransformOn Int a, TransformOn Int b) => TransformOn Int (a, b) where
  transformOn :: (TransformOn Int a, TransformOn Int b) => (Int -> Int) -> (a, b) -> (a, b)
  transformOn f (x, y) = (transformOn f x, transformOn f y)

addDeeply :: TransformOn Int a => Int -> a -> a
addDeeply k = transformOn (+k)

myDeepInts :: (Int, (Int, ((Int, (Int, Int)), Int)))
myDeepInts = (1, (2, ((3, (4, 5)), 6)))

myDeepInts2 :: ((Int, Int), Int)
myDeepInts2 = addDeeply 3 ((3, 4), 1) -- ((6, 7), 1)

myDeepInts' :: (Int, (Int, ((Int, (Int, Int)), Int)))
myDeepInts' = addDeeply 10 myDeepInts -- (11, (12, ((13, (14, 15)), 16)))

