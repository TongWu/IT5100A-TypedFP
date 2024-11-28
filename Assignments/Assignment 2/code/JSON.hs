module JSON where

import Data.List (intercalate)

data JSON = O [(String, JSON)]
          | L [JSON]
          | N Int
          | S String
          | B Bool
          | Null
  deriving Eq

instance Show JSON where
  show :: JSON -> String
  show (O ls) = "{ " ++ (intercalate ", " (map show' ls)) ++ " }"
    where show' (s, j) = show s ++ ": " ++ show j
  show (L ls) = show ls
  show (N x) = show x
  show (S s) = show s
  show (B True) = "true"
  show (B False) = "false"
  show Null = "null"

-- Example JSON value
myJson :: JSON
myJson = O [("name", S "Bob")
          , ("id", N 1234)
          , ("friends", L [S "Alice", S "Charlie", Null])
          , ("courses", O [("IT5100A", O [("completed", B True)
                                        , ("score", N 47)])
                          ,("IT5100B", O [("completed", B False)
                                        , ("score", Null)])])
          , ("email", Null)]

