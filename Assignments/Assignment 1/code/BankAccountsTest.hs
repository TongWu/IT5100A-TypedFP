module BankAccountsTest(bankAccounts) where
import BankAccounts
import Test
import Data.Ratio

-- Comment out any of the following list items if you do not want to test that question
bankAccounts :: TestSuite
bankAccounts = TestSuite "Bank Accounts" [
      accessorFunctions
    , basicFeatures
    , advancedFeatures
  ]

deriving instance Show BankAccount
deriving instance Eq BankAccount

accessorFunctions :: TestGroup
accessorFunctions 
  = TestGroup "Question 1 and 2: Accessor Funnctions" $ do
       let na = NormalAccount "abc" 1000 0.01
           ma = MinimalAccount "bcd" 2000 0.02
           marks = 1 % 3
       ghci "x = NormalAccount \"abc\" 1000 0.01"
       ghci "y = MinimalAccount \"bcd\" 2000 0.02"
       acc_test1 <- testIt "accountId x" "abc" (accountId na) marks
       acc_test2 <- testIt "accountId y" "bcd" (accountId ma) marks
       bal_test1 <- testIt "balance x" 1000 (balance na) marks
       bal_test2 <- testIt "balance y" 2000 (balance ma) marks
       in_test1 <- testIt "interest x" 0.01 (interest na) marks
       in_test2 <- testIt "interest y" 0.02 (interest ma) marks
       return [acc_test1, acc_test2, bal_test1, bal_test2, in_test1, in_test2]

basicFeatures :: TestGroup
basicFeatures = TestGroup "Question 3: Basic Features" $ do
   let x = NormalAccount "abc" 1000 0.01
       y = MinimalAccount "bcd" 2000 0.02
       marks = 1 % 3
   ghci "x = NormalAccount \"abc\" 1000 0.01"
   ghci "y = MinimalAccount \"bcd\" 2000 0.02"
   t1 <- testIt "deposit 1000 x" (NormalAccount "abc" 2000 0.01) (deposit 1000 x) marks
   t2 <- testIt "deduct 1000 x" (True, NormalAccount "abc" 0 0.01) (deduct 1000 x) marks 
   t3 <- testIt "deduct 2001 y" (False, MinimalAccount "bcd" 2000 0.02) (deduct 2001 y) marks
   return [t1, t2, t3]

advancedFeatures :: TestGroup
advancedFeatures = TestGroup "Question 4: Advanced Features" $ do
   let x = NormalAccount "abc" 1000 0.01
       y = MinimalAccount "bcd" 2000 0.02
       z = MinimalAccount "def" 999 0.01
       w = MinimalAccount "xyz" 19 0.01
   ghci "x = NormalAccount \"abc\" 1000 0.01"
   ghci "y = MinimalAccount \"bcd\" 2000 0.02"
   ghci "z = MinimalAccount \"def\" 999 0.01"
   ghci "w = MinimalAccount \"xyz\" 19 0.01"
   t1 <- testIt "compound x" (NormalAccount "abc" (1010 % 1) (1 % 100)) (compound x) marks
   t2 <- testIt "compound (compound x)" (NormalAccount "abc" (10201 % 10) (1 % 100)) (compound (compound x)) marks
   t3 <- testIt "compound y" (MinimalAccount "bcd" (2040 % 1) (1 % 50)) (compound y) marks
   t4 <- testIt "compound z" (MinimalAccount "def" (98879 % 100) (1 % 100)) (compound z) marks
   t5 <- testIt "compound w" (MinimalAccount "xyz" (0 % 1) (1 % 100)) (compound w) marks
   t6 <- testIt "transfer 2000 x y" (False,NormalAccount "abc" (1000 % 1) (1 % 100),MinimalAccount "bcd" (2000 % 1) (1 % 50)) (transfer 2000 x y) marks
   t7 <- testIt "transfer 2000 y x" (True,MinimalAccount "bcd" (0 % 1) (1 % 50), NormalAccount "abc" (3000 % 1) (1 % 100)) (transfer 2000 y x) marks
   return [t1, t2, t3, t4, t5, t6, t7]
  where marks = 2 % 7


