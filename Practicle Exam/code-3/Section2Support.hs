module Section2Support where

-- Basic types
type Balance = Rational
type Username = String
type Inventory = [(ProductName, Quantity)]
type User = (Username, (Balance, Inventory))

type ProductName = String
type Quantity = Int
type Cost = Rational
type Product = (ProductName, (Cost, Quantity))

type Database = ([User], [Product])

-- Example database
exampleDb :: Database
exampleDb = (example_users, example_products) where
  example_users = [
        ("Alice", (100, [("Bow", 1), ("Arrow", 2)]))
      , ("Bob",   (150, [("Arrow", 1)]))
    ]
  example_products = [
        ("Bow", (50, 5))
      , ("Arrow", (10, 10))
    ]

