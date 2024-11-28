import Section3Support

-- TODO: Question 10: write the full program.
import System.Environment (getArgs)
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Maybe

type City = String
type Graph = Map.Map City [City]
type Memo = Map.Map City Int

buildGraph :: String -> Graph
buildGraph contents = Map.fromList $ map parseLine (lines contents)

parseLine :: String -> (City, [City])
parseLine line = case words line of
    [] -> error "Invalid"
    (city:neighbors) -> (city, neighbors)

countPaths :: City -> City -> Graph -> State Memo Int
-- Parses each line into a tuple
countPaths city dest graph = do
    memo <- get
    case Map.lookup city memo of
        Just count -> return count  -- Return cached result
        Nothing -> do
            if city == dest
                then do
                    modify (Map.insert city 1)  -- Base case which 1 path to destination
                    return 1
                else do
                    let neighbors = Map.findWithDefault [] city graph
                    counts <- mapM (\neighbor -> countPaths neighbor dest graph) neighbors
                    let total = sum counts
                    modify (Map.insert city total)
                    return total

main :: IO ()
main = do
    mapFileName <- getLine
    sourceCity <- getLine
    destCity <- getLine

    contents <- readFile mapFileName
    let graph = buildGraph contents

    if Map.member sourceCity graph && Map.member destCity graph
        then do
            let (count, _) = runState (countPaths sourceCity destCity graph) Map.empty
            putStrLn $ show count
        else
            putStrLn $ "0"

-- main :: IO ()
-- main = putStrLn "Hello World!"

