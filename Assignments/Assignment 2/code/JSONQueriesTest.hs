module JSONQueriesTest(jsonQueries) where
import JSON
import JSONOps
import Test
import Data.Ratio

-- Comment out any of the following list items if you do not want to test that question

jsonQueries :: TestSuite
jsonQueries = TestSuite "JSON Queries" [
    gettingNumbers
  , queryingKeys
  , gettingScores
  ]

gettingNumbers :: TestGroup
gettingNumbers = TestGroup "Question 4: Getting Numbers" $ do
  let marks = 1 % 3
  a <- testIt "getN (N 123)" (Just 123) (getN (N 123)) marks
  b <- testIt "getN (S \"123\")" Nothing (getN (S "123")) marks
  c <- testIt "getN (O [(\"number\", N 123)])" Nothing (getN (O [("number", N 123)])) marks
  return [a, b, c]

queryingKeys :: TestGroup
queryingKeys = TestGroup "Question 5: Querying Keys" $ do
  let marks = 2 % 3
  ghci "myJson"
  print myJson
  a <- testIt "queryKey \"name\" myJson" (Just (S "Bob")) (queryKey "name" myJson) marks
  b <- testIt "queryKey \"age\" myJson" Nothing (queryKey "age" myJson) marks
  c <- testIt "queryKey \"x\" (L [O [(\"x\", N 1)]])" Nothing (queryKey "x" (L [O [("x", N 1)]])) marks
  return [a, b, c]

gettingScores :: TestGroup
gettingScores = TestGroup "Question 6: Querying Keys" $ do
  let marks = 1
  ghci "myJson"
  print myJson
  a <- testIt "getScore \"IT5100A\" myJson" (Just 47) (getScore "IT5100A" myJson) marks
  b <- testIt "getScore \"IT5100B\" myJson" Nothing (getScore "IT5100B" myJson) marks
  c <- testIt "getScore \"AB123\" (N 45)" Nothing (getScore "AB123" (N 45)) marks
  return [a, b, c]

