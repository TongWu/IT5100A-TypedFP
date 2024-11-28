module Game where

import Data.Game
import Game.Monad
import Text.Read

-- | Repeatedly reads a line from a user until the user
-- enters a valid blank position on the board.
readPosition :: GameM Int
readPosition = do
  board <- getBoard
  let n = countBlanks board
  let loop = do
        putMsg $ "Please enter a position (1-" ++ show n ++ "): "
        user_input <- readLine
        let mn :: Maybe Int = readMaybe user_input
        case mn of
          Just num ->
            if num >= 1 && num <= n
              then return num
              else do
                putMsg "Invalid position, please try again."
                loop
          Nothing -> do
            putMsg "Invalid input, please enter a numer."
            loop
  loop

-- | Generates a random blank position on the board
randomPosition :: GameM Int
randomPosition = do
    board <- getBoard
    let n = countBlanks board
    x <- randomInt
    return $ (x `mod` n) + 1

-- | Plays a game of monadic tic tac toe
game :: GameM ()
game = do
    status <- getGameStatus
    case status of
        Winner player -> do
            putMsg $ "Game end, the winner is: " ++ show player
            board <- getBoard
            putMsg $ show board
        Draw -> do
            putMsg "Game end, it's a draw."
            board <- getBoard
            putMsg $ show board
        Ongoing Human -> do
            board <- getBoard
            putMsg $ show board
            pos <- readPosition
            setAt pos
            game
        Ongoing Bot -> do
            board <- getBoard
            putMsg $ "Robot is thinking..."
            pos <- randomPosition
            setAt pos
            putMsg $ "Robot placed at position " ++ show pos
            game
