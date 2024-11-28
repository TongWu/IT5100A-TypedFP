{-|
Module      : Data.Game
Description : Basic data types supporting a game of tic-tac-toe
Copyright   : (c) Foo Yong Qi, 2024
License     : MIT
Maintainer  : yongqi@nus.edu.sg
Stability   : experimental

This module contains all data types supporting a game of tic-tac-toe.
In particular, it is a game where humans are always first to play.
-}
module Data.Game(
  -- * The 'GameBoard'
  GameBoard
  -- ** Useful 'GameBoard's
, blankBoard
, testBoard
  -- ** Operations on 'GameBoard's
, countBlanks
, setBlank
  -- * Status of games
, GameStatus(..)
  -- ** Obtaining the status of a game
, gameStatus
  -- * 'Player's
, Player(..)
) where

import Control.Monad.State
    ( evalState, MonadState(put, get), State )
import Control.Applicative

-- | 
-- = Game boards
-- A tic-tac-toe game board. The constructor should be treated as private unless within the 'Data.Game' module. Most operations should make use of 'blankBoard', a 'GameBoard' with no pieces. There is also 'testBoard' which provides a 'GameBoard' for testing only.
--
-- == __Example__
-- >>> blankBoard
-- ┏━━━┳━━━┳━━━┓
-- ┃ 1 ┃ 2 ┃ 3 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 4 ┃ 5 ┃ 6 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 7 ┃ 8 ┃ 9 ┃
-- ┗━━━┻━━━┻━━━┛
-- >>> testBoard
-- ┏━━━┳━━━┳━━━┓
-- ┃ X ┃ O ┃ X ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 1 ┃ X ┃ O ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ X ┃ 2 ┃ O ┃
-- ┗━━━┻━━━┻━━━┛
newtype GameBoard = GameBoard [BoardPosition] deriving Eq

-- | A player in the game.
data Player = Human  -- ^ The human
            | Bot    -- ^ The bot
  deriving (Eq, Show)

-- | A completely blank 'GameBoard'.
blankBoard :: GameBoard
blankBoard = GameBoard $ replicate 9 B

-- | A 'GameBoard' arbitrarily filled with random pieces for testing only. The actual pieces in 'testBoard' are shown in the documentation for 'GameBoard'.
testBoard :: GameBoard
testBoard = GameBoard [P X, P O, P X, B, P X, P O, P X, B, P O]

-- | The number of blank positions in a 'GameBoard'
--
-- == __Examples__
-- >>> countBlanks blankBoard
-- 9
-- >>> countBlanks testBoard
-- 2
countBlanks :: GameBoard -- ^ The 'GameBoard'
            -> Int       -- ^ The number of blank positions in the 'GameBoard'
countBlanks (GameBoard ls) = length $ filter (== B) ls

-- | Sets the `n`th blank position in the 'GameBoard' with the current player's piece.
-- If `n` is out of bounds, then the 'GameBoard' is returned unmodified.
--
-- == __Examples__
-- >>> setBlank 9 blankBoard
-- ┏━━━┳━━━┳━━━┓
-- ┃ 1 ┃ 2 ┃ 3 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 4 ┃ 5 ┃ 6 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 7 ┃ 8 ┃ X ┃
-- ┗━━━┻━━━┻━━━┛
-- ghci> setBlank (-1) blankBoard
-- ┏━━━┳━━━┳━━━┓
-- ┃ 1 ┃ 2 ┃ 3 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 4 ┃ 5 ┃ 6 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 7 ┃ 8 ┃ 9 ┃
-- ┗━━━┻━━━┻━━━┛
-- ghci> setBlank 2 testBoard
-- ┏━━━┳━━━┳━━━┓
-- ┃ X ┃ O ┃ X ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 1 ┃ X ┃ O ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ X ┃ O ┃ O ┃
-- ┗━━━┻━━━┻━━━┛
setBlank :: Int       -- ^ `n`, i.e. the position to set
         -> GameBoard -- ^ The 'GameBoard' to set
         -> GameBoard -- ^ The resulting 'GameBoard' where the `n`th position is set with the current player's piece
setBlank n b@(GameBoard ls)
    | n < 1 || n > countBlanks b = b
    | otherwise                  = GameBoard $ aux n ls
  where aux :: Int -> [BoardPosition] -> [BoardPosition]
        aux 1 (B : xs)
          | odd (countBlanks b) = P X : xs
          | otherwise           = P O : xs
        aux n' (B : xs) = B : aux (n' - 1) xs
        aux n' (x : xs) = x : aux n' xs
        aux _ _ = undefined


-- | The status of the current game
data GameStatus = Winner    -- ^ The game has concluded with a winner
                    Player  -- ^ The winning player
                | Draw      -- ^ The game has concluded with a draw
                | Ongoing   -- ^ The game is still ongoing
                    Player  -- ^ The current player's turn
  deriving (Eq, Show)

-- | Obtains the status of the game based on the 'GameBoard'
--
-- == __Examples__
-- >>> gameStatus blankBoard
-- Ongoing Human
-- >>> gameStatus testBoard
-- Winner Human
gameStatus :: GameBoard  -- ^ The 'GameBoard'
           -> GameStatus -- ^ The status of 'GameBoard'
gameStatus b = 
  case winner b of
    Just x -> Winner x
    Nothing -> if countBlanks b == 0
               then Draw
               else if odd (countBlanks b)
               then Ongoing Human
               else Ongoing Bot

data Piece = X | O
  deriving Eq

instance Show Piece where
  show :: Piece -> String
  show X = "X" -- "⨉"
  show O = "O" -- "◯"

piece'sPlayer :: Piece -> Player
piece'sPlayer X = Human
piece'sPlayer O = Bot

data BoardPosition = P Piece | B deriving Eq

positionPiece :: BoardPosition -> Maybe Piece
positionPiece (P p) = Just p
positionPiece B = Nothing

showBoardPosition :: BoardPosition -> State Int String
showBoardPosition (P x) = return (show x)
showBoardPosition B 
  = do n <- get
       put (n + 1)
       return $ show n

rows :: [a] -> ([a], [a], [a])
rows ls = let r1 = take 3 ls
              r2 = take 3 $ drop 3 ls
              r3 = take 3 $ drop 6 ls
          in  (r1, r2, r3)

columns :: [a] -> ([a], [a], [a])
columns ls = ([ls !! i | i <- [0, 3, 6]],
              [ls !! i | i <- [1, 4, 7]],
              [ls !! i | i <- [2, 5, 8]])

diagonals :: [a] -> ([a], [a])
diagonals ls = ([ls !! i | i <- [0, 4, 8]],
                [ls !! i | i <- [2, 4, 6]])

winningTriplet :: [BoardPosition] -> Maybe Piece
winningTriplet [x, y, z] =
  if x == y && y == z
  then positionPiece x
  else Nothing
winningTriplet _ = Nothing

instance Show GameBoard where
  show :: GameBoard -> String
  show (GameBoard ls) = 
      let pieceString = evalState (mapM showBoardPosition ls) 1
          (r1, r2, r3) = rows pieceString
      in  "┏━━━┳━━━┳━━━┓\n" ++ 
            showRow r1 ++ 
            "┣━━━╋━━━╋━━━┫\n" ++ 
            showRow r2 ++ 
            "┣━━━╋━━━╋━━━┫\n" ++ 
            showRow r3 ++ 
            "┗━━━┻━━━┻━━━┛"
    where showRow [a, b, c] = "┃ " ++ a ++ " ┃ " ++ b ++ " ┃ " ++ c ++ " ┃\n"
          showRow _ = undefined

winner :: GameBoard -> Maybe Player
winner (GameBoard ls) = 
  let (r1, r2, r3) = rows ls
      (c1, c2, c3) = columns ls
      (d1, d2)     = diagonals ls
      all_triplets = [r1, r2, r3, c1, c2, c3, d1, d2]
  in  piece'sPlayer <$> foldl (<|>) Nothing (map winningTriplet all_triplets)

