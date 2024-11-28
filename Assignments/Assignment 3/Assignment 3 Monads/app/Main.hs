module Main where
import Data.Game
import Game.Monad
import Game
import System.CPUTime (getCPUTime)

main :: IO ()
main = playRandomGame

playRandomGame :: IO ()
playRandomGame = do
  seed <- getCPUTime
  evalStateT game (blankBoard, seed + 987654321)

testGetsPuts :: IO ()
testGetsPuts = 
  let f = do
        b <- getBoard
        s <- getSeed
        putMsg "Board:"
        putMsg $ show b
        putMsg $ "Seed: " ++ show s
        putMsg "Updating board to testBoard"
        putMsg "Increasing seed by 1"
        putBoard testBoard
        putSeed (s + 1)
        b' <- getBoard
        s' <- getSeed
        putMsg "New board:"
        putMsg $ show b'
        putMsg $ "New seed: " ++ show s'
        putMsg "Enter something in the console:"
        l <- readLine
        putMsg $ "Got: " ++ show l
  in evalStateT f (blankBoard, 1)

testGameStatus :: IO ()
testGameStatus = do
  x <- evalStateT getGameStatus (blankBoard, 1)
  y <- evalStateT getGameStatus (testBoard, 1)
  print blankBoard
  print x
  print testBoard
  print y

testRandomInt :: IO ()
testRandomInt =
  let f = do
        x <- randomInt
        y <- randomInt
        z <- randomInt
        putMsg $ show x
        putMsg $ show y
        putMsg $ show z
  in  evalStateT f (blankBoard, 987654321)

testSetAt :: IO ()
testSetAt = 
  let f = do
        board <- getBoard
        putMsg "Current board:"
        putMsg $ show board
        putMsg "Setting position 1"
        setAt 1
        newBoard <- getBoard
        putMsg "New board"
        putMsg $ show newBoard
  in  evalStateT f (blankBoard, 1) >> evalStateT f (testBoard, 1)

