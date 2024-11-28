-- Solutions Manual
module BankAccounts(
  BankAccount(..)
  , accountId
  , balance
  , interest
  , deposit
  , deduct
  , compound
  , transfer
  , BSTMap(..)
  , put
  , get
  , in'
  , values
  , Op(..)
  , processOne
  , processAll
  , process
) where

data BankAccount = NormalAccount String Rational Rational
                 | MinimalAccount String Rational Rational

accountId :: BankAccount -> String
accountId (NormalAccount s _ _) = s
accountId (MinimalAccount s _ _) = s

balance :: BankAccount -> Rational
balance (NormalAccount _ x _) = x
balance (MinimalAccount _ x _) = x

interest :: BankAccount -> Rational
interest (NormalAccount _ _ x) = x
interest (MinimalAccount _ _ x) = x

setBalance :: Rational -> BankAccount -> BankAccount
setBalance x (NormalAccount a _ c) = NormalAccount a x c
setBalance x (MinimalAccount a _ c) = MinimalAccount a x c

deposit :: Rational -> BankAccount -> BankAccount
deposit x b = let newBalance = x + balance b 
              in  setBalance newBalance b

deduct :: Rational -> BankAccount -> (Bool, BankAccount)
deduct x b 
    | bal < x   = (False, b)
    | otherwise = (True, setBalance (bal - x) b)
  where bal = balance b

transfer :: Rational -> BankAccount -> BankAccount -> (Bool, BankAccount, BankAccount)
transfer x from to = (res, from', to')
  where (res, from') = deduct x from
        to' | res       = deposit x to
            | otherwise = to

compound :: BankAccount -> BankAccount
compound (NormalAccount s bal ir) = NormalAccount s (bal + bal * ir) ir
compound (MinimalAccount s bal ir) = MinimalAccount s bal'' ir
  where bal' | bal < 1000 = max (bal - 20) 0
             | otherwise  = bal
        bal'' = bal' + bal' * ir

data BSTMap k v = Empty
                | Node (BSTMap k v) k v (BSTMap k v)

put :: Ord a => a -> b -> BSTMap a b -> BSTMap a b
put k v Empty = Node Empty k v Empty
put k v (Node l k' v' r)
  | k == k' = Node l k v r
  | k < k'  = Node (put k v l) k' v' r
  | otherwise = Node l k' v' (put k v r)

{-# WARNING get "This is a partial function; it is undefined when the key is not present in the map." #-}
get :: Ord a => a -> BSTMap a b -> b
get _ Empty = undefined
get k (Node l k' v r)
  | k == k' = v
  | k < k'  = get k l
  | otherwise = get k r

in' :: Ord a => a -> BSTMap a b -> Bool
in' _ Empty = False
in' k (Node l k' _ r)
  | k == k' = True
  | k < k'  = in' k l
  | otherwise = in' k r

values :: BSTMap a b -> [b]
values Empty = []
values (Node l _ v r) = values l ++ (v : values r)

compoundAll :: BSTMap String BankAccount -> BSTMap String BankAccount
compoundAll Empty = Empty
compoundAll (Node l k v r) = Node (compoundAll l) k (compound v) (compoundAll r)

transfer' :: Rational -> String -> String -> BSTMap String BankAccount -> (Bool, BSTMap String BankAccount)
transfer' x from to b
  | not (from `in'` b) = (False, b)
  | not (to `in'` b)   = (False, b)
  | otherwise = 
      let from' = get from b
          to'   = get to b
          (res, f, t) = transfer x from' to'
          db    = put from f $ put to t b
      in  (res, db)
 
data Op = Transfer Rational String String 
        | Compound

processOne :: Op -> BSTMap String BankAccount -> (Bool, BSTMap String BankAccount)
processOne Compound = (True,) . compoundAll
processOne (Transfer x from to) = transfer' x from to

processAll :: [Op] -> BSTMap String BankAccount -> ([Bool], BSTMap String BankAccount)
processAll [] b = ([], b)
processAll (x : xs) b = 
  let (opR, b') = processOne x b
      (rs, b'') = processAll xs b'
  in  (opR : rs, b'')

process :: (a -> b -> (c, b)) -> [a] -> b -> ([c], b)
process _ [] a = ([], a)
process f (x : xs) a = 
  let (x', a') = f x a
      (xs', a'') = process f xs a'
  in  (x' : xs', a'')
