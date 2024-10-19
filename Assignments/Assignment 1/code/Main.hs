module Main where

import Test ( box, displayRatio, TestSuite, test )

-- Comment out the import statements for sections that you do not want to test
import BankAccountsTest -- section 1
-- import TablesTest       -- section 2
-- import OpTest           -- section 3

assignment1 :: [TestSuite]
-- Each item in the following list is a section in Assignment 1. Comment
-- that section out if you do not want to test the questions in that section.
assignment1 = [
    bankAccounts -- section 1
  -- , tables       -- section 2
  -- , operations   -- section 3
  ]
  
main :: IO ()
main = do
  (a, t) <- test assignment1
  putStrLn ""
  putStrLn $ box ["Total score: " ++ show (floor a) ++ " out of " ++ displayRatio t]

