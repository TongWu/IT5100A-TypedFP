{-|
Module      : Game.Monad
Description : The Game Monad
Copyright   : (c) IT5100A Student, 2024
License     : MIT
Stability   : experimental

This module defines the main game monad.
-}
module Game.Monad(
  RandomSeed
, GameState
, GameM
, getBoard
, getSeed
, putBoard
, putSeed
, putMsg
, readLine
, getGameStatus
, setAt
, randomInt
, evalStateT
) where
import Data.Game
import Control.Monad.State 

-- | The seed for random number generation. We use 'Integer's for seeds
type RandomSeed = Integer
-- | The 'GameM' monad uses a 'GameBoard' and a 'RandomSeed' as its state.
type GameState = (GameBoard, RandomSeed)
-- | The 'GameM' monad is a 'StateT' monad transformer over the 'IO' monad that uses 'GameState' for its state.
-- Thus, we can use the 'StateT' methods like 'put', 'get' and 'evalStateT' on it.
type GameM = StateT GameState IO

-- | Get the current 'GameBoard'
getBoard :: GameM GameBoard
getBoard = do
  (board, _) <- get
  return board

-- | Get the current 'RandomSeed'
getSeed :: GameM RandomSeed
getSeed = do
  (_, seed) <- get
  return seed

-- | Put a new 'GameBoard'
putBoard :: GameBoard -> GameM ()
putBoard newBoard = do
  (_, seed) <- get
  put (newBoard, seed)

-- | Put a new 'RandomSeed'
putSeed :: RandomSeed -> GameM ()
putSeed newSeed = do
  (board, _) <- get
  put (board, newSeed)

-- | Prints a string to the console
putMsg :: String -> GameM ()
putMsg msg = liftIO $ putStrLn msg

-- | Reads a line from user input in the console
readLine :: GameM String
readLine = liftIO getLine

-- | Get the current 'GameStatus'
getGameStatus :: GameM GameStatus
getGameStatus = do
  board <- getBoard
  return $ gameStatus board

-- | Generates a new random 'Int'
randomInt :: GameM Int
randomInt = do
  s <- getSeed
  let newSeed = (5 * s) `mod` 12356713
      randNum = fromIntegral (s `mod` 133711)
  putSeed newSeed
  return randNum

-- | Sets the `n`th blank position on the board with the 
-- current player's piece
setAt :: Int -> GameM ()
setAt n = do
  board <- getBoard
  let newBoard = setBlank n board
  putBoard newBoard

