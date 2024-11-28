module BankAccounts where

-- TODO: Define the BankAccount ADT.
data BankAccount =
    NormalAccount String Rational Rational
    | MinimalAccount String Rational Rational 

-- TODO: Define the accountId function. 
accountId :: BankAccount -> String
accountId (NormalAccount id _ _) = id
accountId (MinimalAccount id _ _) = id

-- TODO: Define the balance function.
balance :: BankAccount -> Rational
balance (NormalAccount _ bal _) = bal
balance (MinimalAccount _ bal _) = bal

-- TODO: Define the interest function.
interest :: BankAccount -> Rational
interest (NormalAccount _ _ rate) = rate
interest (MinimalAccount _ _ rate) = rate

-- TODO: Define the deposit function.
deposit :: Rational -> BankAccount -> BankAccount
deposit amount (NormalAccount id bal rate) = NormalAccount id (bal + amount) rate
deposit amount (MinimalAccount id bal rate) = MinimalAccount id (bal + amount) rate

-- TODO: Define the deduct function.
deduct :: Rational -> BankAccount -> (Bool, BankAccount)
deduct amount acc@(NormalAccount id bal rate)
    | amount <= bal = (True, NormalAccount id (bal - amount) rate)
    | otherwise = (False, acc)
deduct amount acc@(MinimalAccount id bal rate)
    | amount <= bal = (True, MinimalAccount id (bal - amount) rate)
    | otherwise = (False, acc)

-- TODO: Define the transfer function
transfer :: Rational -> BankAccount -> BankAccount -> (Bool, BankAccount, BankAccount)
transfer amount debitAcc creditAcc =
    case deduct amount debitAcc of
        (True, updatedDebitAcc) -> (True, updatedDebitAcc, deposit amount creditAcc)
        (False, _) -> (False, debitAcc, creditAcc)

-- TODO: Define the compound function.
compound :: BankAccount -> BankAccount
compound (NormalAccount id bal rate) = NormalAccount id (bal * (1 + rate)) rate
compound (MinimalAccount id bal rate)
    | bal < 1000 = MinimalAccount id ((max 0 (bal - 20)) * (1 + rate)) rate
    | otherwise = MinimalAccount id (bal * (1 + rate)) rate

-- TODO: Define the BSTMap ADT
data BSTMap k v =
    Empty
    | Node (BSTMap k v) k v (BSTMap k v)

-- TODO: Define the put function.
put :: Ord k => k -> v -> BSTMap k v -> BSTMap k v
put key value Empty = Node Empty key value Empty
put key value (Node left k v right)
    | key < k = Node (put key value left) k v right
    | key > k = Node left k v (put key value right)
    | otherwise = Node left key value right

-- TODO: Define the get function.
get :: (Ord k, Num v) => k -> BSTMap k v -> v
get _ Empty = 0
get key (Node left k v right)
    | key == k  = v
    | key < k   = get key left
    | otherwise = get key right

-- TODO: Define the in' function.
in' :: Ord k => k -> BSTMap k v -> Bool
in' _ Empty = False
in' key (Node left k v right)
    | key == k = True
    | key < k = in' key left
    | otherwise = in' key right

-- TODO: Define the values function.
values :: BSTMap k v -> [v]
values Empty = []
values (Node left _ v right) = values left ++ [v] ++ values right

-- TODO: Define the Op ADT
data Op =
    Compound
    | Transfer Rational String String

-- Helper function to map over BSTMap
mapBSTMap :: (v -> v) -> BSTMap k v -> BSTMap k v
mapBSTMap _ Empty = Empty
mapBSTMap f (Node left k v right) =
    Node (mapBSTMap f left) k (f v) (mapBSTMap f right)

-- TODO: Define the processOne function.
processOne :: Op -> BSTMap String BankAccount -> (Bool, BSTMap String BankAccount)
processOne Compound bst = (True, mapBSTMap compound bst)
processOne (Transfer amount fromID toID) bst =
    case (getAcc fromID bst, getAcc toID bst) of
        (Just fromAcc, Just toAcc) ->
            case transfer amount fromAcc toAcc of
                (True, updatedFromAcc, updatedToAcc) ->
                    let bst1 = put fromID updatedFromAcc bst
                        bst2 = put toID updatedToAcc bst1
                    in (True, bst2)
                (False, _, _) ->
                    (False, bst)
        _ -> (False, bst)
  where
    getAcc :: String -> BSTMap String BankAccount -> Maybe BankAccount
    getAcc _ Empty = Nothing
    getAcc key (Node left k v right)
        | key == k  = Just v
        | key < k   = getAcc key left
        | otherwise = getAcc key right

-- TODO: Define the processAll function.
processAll :: [Op] -> BSTMap String BankAccount -> ([Bool], BSTMap String BankAccount)
processAll ops bst = 
    let (successesReversed, finalBst) = foldl processOp ([], bst) ops
    in (reverse successesReversed, finalBst)
  where
    processOp (successes, bst) op =
        let (success, bst') = processOne op bst
        in (success : successes, bst')

-- TODO: Define the process function.
process :: (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
process f xs s = 
    let (bsReversed, finalState) = foldl processElement ([], s) xs
    in (reverse bsReversed, finalState)
  where
    processElement (bs, state) x =
        let (b, newState) = f x state
        in (b : bs, newState)

