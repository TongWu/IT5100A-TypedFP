module OpTest(operations) where

import BankAccounts
import Test
import Data.Ratio
import BankAccountsTest
import TablesTest

-- Comment out any of the following list items if you do not want to test that question
operations :: TestSuite 
operations = TestSuite "Operating on Bank Accounts" [
      processOneTest
    , processAllTest
    , processTest
  ]

deriving instance Eq Op
deriving instance Show Op

processOneTest :: TestGroup
processOneTest = TestGroup "Questions 10 and 11: Op + Processing One Operation" $ do
    let alice = NormalAccount "alice" 1000 0.1
        bob   = MinimalAccount "bob" 999 0.1
        mp    = put "alice" alice (put "bob" bob Empty)
        c     = Compound
        t1    = Transfer 1000 "alice" "bob"
        t2    = Transfer 1000 "bob" "alice"
    ghci "alice = NormalAccount \"alice\" 1000 0.1"
    ghci "bob   = MinimalAccount \"bob\" 999 0.1"
    ghci "mp    = put \"alice\" alice (put \"bob\" bob Empty)"
    ghci "c     = Compound"
    ghci "t1    = Transfer 1000 \"alice\" \"bob\""
    ghci "t2    = Transfer 1000 \"bob\" \"alice\""
    r1 <- testIt "processOne c mp" (True,Node (Node Empty "alice" (NormalAccount "alice" (1100 % 1) (1 % 10)) Empty) "bob" (MinimalAccount "bob" (10769 % 10) (1 % 10)) Empty) (processOne c mp) marks
    r2 <- testIt "processOne t1 mp" (True,Node (Node Empty "alice" (NormalAccount "alice" (0 % 1) (1 % 10)) Empty) "bob" (MinimalAccount "bob" (1999 % 1) (1 % 10)) Empty) (processOne t1 mp) marks
    r3 <- testIt "processOne t2 mp" (False,Node (Node Empty "alice" (NormalAccount "alice" (1000 % 1) (1 % 10)) Empty) "bob" (MinimalAccount "bob" (999 % 1) (1 % 10)) Empty) (processOne t2 mp) marks
    return [r1, r2, r3]
  where marks = 1

processAllTest :: TestGroup
processAllTest = TestGroup "Question 12: Processing All Operations" $ do
    let alice = NormalAccount "alice" 1000 0.1
        bob   = MinimalAccount "bob" 999 0.1
        mp    = put "alice" alice (put "bob" bob Empty)
        c     = Compound
        t1    = Transfer 1000 "alice" "bob"
        t2    = Transfer 1000 "bob" "alice"
    ghci "alice = NormalAccount \"alice\" 1000 0.1"
    ghci "bob   = MinimalAccount \"bob\" 999 0.1"
    ghci "mp    = put \"alice\" alice (put \"bob\" bob Empty)"
    ghci "c     = Compound"
    ghci "t1    = Transfer 1000 \"alice\" \"bob\""
    ghci "t2    = Transfer 1000 \"bob\" \"alice\""
    r <- testIt "processAll [t2,c,t2,t1] mp" ([False,True,True,True], Node (Node Empty "alice" (NormalAccount "alice" (1100 % 1) (1 % 10)) Empty) "bob" (MinimalAccount "bob" (10769 % 10) (1 % 10)) Empty) (processAll [t2,c,t2,t1] mp) marks
    return [r]
  where marks = 2

processTest :: TestGroup
processTest = TestGroup "Question 13: Polymorphic Processing" $ do
    let alice = NormalAccount "alice" 1000 0.1
        bob   = MinimalAccount "bob" 999 0.1
        mp    = put "alice" alice (put "bob" bob Empty)
        c     = Compound
        t1    = Transfer 1000 "alice" "bob"
        t2    = Transfer 1000 "bob" "alice"
    ghci "alice = NormalAccount \"alice\" 1000 0.1"
    ghci "bob   = MinimalAccount \"bob\" 999 0.1"
    ghci "mp    = put \"alice\" alice (put \"bob\" bob Empty)"
    ghci "c     = Compound"
    ghci "t1    = Transfer 1000 \"alice\" \"bob\""
    ghci "t2    = Transfer 1000 \"bob\" \"alice\""
    r1 <- testIt "process processOne [t2,c,t2,t1] mp" ([False,True,True,True], Node (Node Empty "alice" (NormalAccount "alice" (1100 % 1) (1 % 10)) Empty) "bob" (MinimalAccount "bob" (10769 % 10) (1 % 10)) Empty) (process processOne [t2,c,t2,t1] mp) marks
    putStrLn "ghci> :{\nghci| divides :: Int -> Int -> Bool\nghci| divides a b = b `mod` a == 0\nghci> :}"
    putStrLn "ghci> :{\nghci| gatherPrime :: Int -> [Int] -> (Bool, [Int])\nghci| gatherPrime n [] = (True, [n])\nghci| gatherPrime n ls\nghci|   | any (`divides` n) ls = (False, ls)\nghci|   | otherwise    = (True, ls ++ [n])\nghci| :}"
    r2 <- testIt "process gatherPrime [2..10] []" ([True, True, False, True, False, True, False, False, False], [2, 3, 5, 7]) (process gatherPrime [2..10] []) marks
    return [r1, r2]
  where marks = 3 % 2
        divides :: Int -> Int -> Bool
        divides a b = b `mod` a == 0

        gatherPrime :: Int -> [Int] -> (Bool, [Int])
        gatherPrime n [] = (True, [n])
        gatherPrime n ls
          | any (`divides` n) ls = (False, ls)
          | otherwise    = (True, ls ++ [n])

