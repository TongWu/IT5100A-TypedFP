module TablesTest(tables) where

import BankAccounts
import Test
import Data.Ratio

-- Comment out any of the following list items if you do not want to test that question
tables :: TestSuite 
tables = TestSuite "Tables" [
      putTest
    , inTest
    , getTest
    , valuesTest
  ]

deriving instance (Eq a, Eq b) => Eq (BSTMap a b)
deriving instance (Show a, Show b) => Show (BSTMap a b)

putTest :: TestGroup
putTest = TestGroup "Question 5 and 6: BST + Put" $ do
    t1 <- testIt "put 1 2 Empty" (Node Empty 1 2 Empty) (put 1 2 Empty) marks
    t2 <- testIt "put 2 3 $ put 1 2 Empty" (Node Empty 1 2 (Node Empty 2 3 Empty)) (put 2 3 $ put 1 2 Empty) marks
    t3 <- testIt "put 0 2 $ put 2 3 $ put 1 2 Empty" (Node (Node Empty 0 2 Empty) 1 2 (Node Empty 2 3 Empty)) (put 0 2 $ put 2 3 $ put 1 2 Empty) marks
    t4 <- testIt "put 1 4 $ put 0 2 $ put 2 3 $ put 1 2 Empty" (Node (Node Empty 0 2 Empty) 1 4 (Node Empty 2 3 Empty)) (put 1 4 $ put 0 2 $ put 2 3 $ put 1 2 Empty) marks
    return [t1, t2, t3, t4]
  where marks = 3 % 4

inTest :: TestGroup
inTest = TestGroup "Question 7: Key Membership" $ do
    let mp = put 1 4 $ put 0 2 $ put 2 3 (put 1 2 Empty)
    ghci "mp = put 1 4 $ put 0 2 $ put 2 3 (put 1 2 Empty)"
    t1 <- testIt "0 `in'` mp"  True (0 `in'` mp) marks
    t2 <- testIt "3 `in'` mp" False (3 `in'` mp) marks
    return [t1, t2]
  where marks = 1 % 2

getTest :: TestGroup
getTest = TestGroup "Question 8: Obtaining Values" $ do
    let mp = put 0 2 $ put 2 3 $ put 1 2 Empty
    ghci "mp = put 0 2 $ put 2 3 $ put 1 2 Empty"
    t1 <- testIt "get 0 mp" 2 (get 0 mp) marks
    t2 <- testIt "get 2 mp" 3 (get 2 mp) marks
    t3 <- testIt "get 1 mp" 2 (get 1 mp) marks
    return [t1, t2, t3]
  where marks = 1 % 3

valuesTest :: TestGroup
valuesTest = TestGroup "Question 9: Obtaining All Values" $ do
    let mp = put 0 3 $ put 1 2 $ put 2 1 Empty
    ghci "mp = put 0 3 $ put 1 2 $ put 2 1 Empty"
    t <- testIt "values mp" [3, 2, 1] (values mp) 2
    return [t]
