module Main where

import Test ( box, displayRatio, TestSuite, test )

-- Comment out the import statements for sections that you do not want to test
import TransformOnTest -- section 1
import JSONQueriesTest -- section 2

assignment2 :: [TestSuite]
-- Each item in the following list is a section in Assignment 2. Comment
-- that section out if you do not want to test the questions in that section.
assignment2 = [
    transformations -- section 1
  , jsonQueries     -- section 2
  ]
  
main :: IO ()
main = do
  (a, t) <- test assignment2
  putStrLn ""
  putStrLn $ box ["Total score: " ++ show (floor a) ++ " out of " ++ displayRatio t]

