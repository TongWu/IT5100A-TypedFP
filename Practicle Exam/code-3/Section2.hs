import Section2Support
-- feel free to use 'guard' from 'Control.Monad'
import Control.Monad 
-- import other things too if you want

-- TODO: Question 5: define the 'update' function
update :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
update key value [] = [(key, value)]
update key value ((k, v):xs)
    | key == k  = (key, value) : xs  -- Update the existing key-value pair
    | otherwise = (k, v) : update key value xs  -- Keep searching for the key

-- TODO: Question 6: define the 'deductUserBalance' function
deductUserBalance :: Username -> Balance -> Database -> Maybe Database
deductUserBalance username amount (users, products) = do
    guard (amount > 0) -- Ensure the amount is positive
    (balance, inventory) <- lookup username users   -- Find the user in the database
    guard (balance >= amount)
    let newBalance = balance - amount
    let newUsers = update username (newBalance, inventory) users -- Update the user's information in the users list
    return (newUsers, products)

-- TODO: Question 7: define the 'deductProductQuantity' function
deductProductQuantity :: ProductName -> Quantity -> Database -> Maybe Database
deductProductQuantity n q (users, products) = do
    guard (q > 0)
    (cost, qty) <- lookup n products
    guard (qty >= q)
    let newQty = qty - q -- Calculate the new quantity
    -- Update products list
    let newProducts = if newQty > 0
        then update n (cost, newQty) products
        else filter (\(pn, _) -> pn /= n) products
    return (users, newProducts)


-- TODO: Question 8: define the 'addProductToUser' function
addProductToUser :: ProductName -> Quantity -> Username -> Database -> Maybe Database
addProductToUser p q u (users, products) = do
    guard (q > 0)
    (balance, inventory) <- lookup u users
    let oldQty = maybe 0 id (lookup p inventory)
    let newQty = oldQty + q
    let newInventory = update p newQty inventory
    let newUsers = update u (balance, newInventory) users
    return (newUsers, products)


-- TODO: Question 9: define the 'userPurchaseProduct' function
userPurchaseProduct :: Username -> ProductName -> Quantity -> Database -> Maybe Database
userPurchaseProduct u p q db = do
    guard (q > 0)
    let (users, products) = db
    (costPerItem, productQty) <- lookup p products
    let totalCost = costPerItem * fromIntegral q    -- Compute total cost
    db1 <- deductUserBalance u totalCost db -- Deduct user balance
    db2 <- deductProductQuantity p q db1    -- Deduct product quantity
    db3 <- addProductToUser p q u db2   -- Add product to user
    return db3