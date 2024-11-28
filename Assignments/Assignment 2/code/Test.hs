-- This code sucks; 
{-# LANGUAGE ScopedTypeVariables #-}
module Test(TestSuite(TestSuite)
, TestGroup(TestGroup)
, testIt
, ghci
, test
, displayRatio
, box
) where

import Data.Ratio
import Control.Exception (try, evaluate, SomeException (SomeException))
import Data.List (intercalate, intersperse)

ghci :: String -> IO ()
ghci e = putStrLn $ "ghci> " ++ e

data TestGroup = TestGroup { group_name :: String, tests :: IO [(Rational, Rational)] } | GroupDelimiter
data TestSuite = TestSuite { suite_name :: String, groups :: [TestGroup] } | SuiteDelimiter

class Testable a where
  test :: a -> IO (Rational, Rational)

instance Testable [TestGroup] where
  test :: [TestGroup] -> IO (Rational, Rational)
  test ls = do results <- mapM test $ intersperse GroupDelimiter ls
               let attained = sum $ map fst results
                   total    = sum $ map snd results
               return (attained, total)

instance Testable [TestSuite] where
  test :: [TestSuite] -> IO (Rational, Rational)
  test ls = do results <- mapM test $ intersperse SuiteDelimiter ls
               let attained = sum $ map fst results
                   total    = sum $ map snd results
               return (attained, total)

instance Testable TestSuite where
  test :: TestSuite -> IO (Rational, Rational)
  test SuiteDelimiter = putStrLn "" >> putStrLn "============" >> putStrLn "" >> return (0, 0)
  test s = do
    putStrLn $ box ["Test Suite: " ++ suite_name s]
    putStrLn "-----------"
    (a, b) <- test $ groups s
    putStrLn "-----------"
    putStrLn $ "Suite score: " ++ displayRatio a ++ " out of " ++ displayRatio b
    return (a, b)

instance Testable TestGroup where
  test :: TestGroup -> IO (Rational, Rational)
  test GroupDelimiter = putStrLn "\n" >> return (0, 0)
  test t = do
    putStrLn $ box ["Test: " ++ group_name t]
    t' <- tests t
    let (a, b) = collectTestResults t'
    putStrLn $ "Test score: " ++ displayRatio a ++ " out of " ++ displayRatio b
    return (a, b)

testIt :: forall a. (Show a, Eq a) => String -> a -> a -> Rational -> IO (Rational, Rational)
testIt s e o i = do
  putStrLn $ "TEST> " ++ s
  putStrLn $ "Expected: " ++ show e
  o' <- try (evaluate o) :: IO (Either SomeException a)
  case o' of
    Left ex -> do putStrLn "Exception caught!"
                  print ex
                  putStrLn  "Score: 0\n"
                  return (0, i)
    Right val -> do putStrLn $ "Output: " ++ show val
                    if e == val
                    then putStrLn ("Score: " ++ displayRatio i) >> putStrLn "" >> return (i, i)
                    else putStrLn "Score: 0\n" >> return (0, i)

collectTestResults :: Num a => [(a, a)] -> (a, a)
collectTestResults = foldr f (0, 0)
  where f (a, b) (c, d) = (a + c, b + d)

displayRatio :: (Num a, Show a, Eq a) => Ratio a -> String
displayRatio s = if denominator s == 1 then show $ numerator s else show s

rightPad :: Int -> String -> String
rightPad i s = s ++ replicate (i - length s) ' '

boxRow :: Int -> String -> String
boxRow maxWidth s = "┃ " ++ rightPad maxWidth s ++ " ┃"

box :: [String] -> String
box ls = let top = "┏" ++ replicate (width + 2) '━' ++ "┓\n"
             mids = intercalate "\n" $ map (boxRow width) ls
             bot = "\n┗" ++ replicate (width + 2) '━' ++ "┛"
         in top ++ mids ++ bot
  where width = foldr (max . length) 0 ls

