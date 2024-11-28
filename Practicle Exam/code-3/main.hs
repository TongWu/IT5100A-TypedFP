Question 4 (Will It Open?) [3 marks]. It is finally time to solve the problem. Write a function
willOpen that receives:
1. the current state of the dial as an Int between 0 and 359 inclusive,
2. a safe password represented as a list of Ints (each between 0 and 359 inclusive),
3. a list of rotations represented as a list of Ints where negative numbers represent counterclock-
wise rotations and positive numbers represent clockwise rotations, and 0 means “no rotation”,
and returns True if the safe is unlocked after completing all the specified rotations, False otherwise.
Example runs follow.
ghci> willOpen 0 [] [1] -- empty password
True
ghci> willOpen 359 [0] [1]
True
ghci> willOpen 1 [0] [-1] -- unlocking sequence always start with clockwise rotation
False
ghci> willOpen 2 [1, 0, 1] [-2, 1,
-1, 1]
-- first rotation brings dial to 0, next three rotations unlock safe
True
ghci> willOpen 2 [1, 0, 1] [-2, 1,
-1, 1,
-360]
-- second-last rotation unlocks safe, but last counterclockwise rotation
-- re-locks safe
False
ghci> willOpen 2 [1, 0, 1] [-2, 1,
-1, 2, 719]
-- second-last rotation and last rotation is the same as one clockwise rotation of
-- 1 degree
True


Tip: Use toRotations and unlockSequence to help you!

-- TODO: Question 1: define the Rotation ADT
data Rotation = CW Int | CCW Int deriving (Show, Eq)

-- TODO: Question 2: define the 'toRotations' function
toRotations :: [Int] -> [Rotation]
toRotations xs = reverse (process xs Nothing [])

process :: [Int] -> Maybe (Bool, Int) -> [Rotation] -> [Rotation]
process [] Nothing res = res
process [] (Just (dir, amt)) res = normalizeAndAdd dir amt res
process (x:xs) acc res
    | x == 0 = process xs acc res  -- Ignore zeros
    | otherwise =
        let dir = x > 0  -- True for CW, False for CCW
            amt = x
        in case acc of
            Nothing -> process xs (Just (dir, amt)) res
            Just (prevDir, prevAmt) ->
                if dir == prevDir then
                    process xs (Just (dir, prevAmt + amt)) res  -- Accumulate rotation
                else
                    let res' = normalizeAndAdd prevDir prevAmt res  -- Output previous rotation
                    in process xs (Just (dir, amt)) res'

normalizeAndAdd :: Bool -> Int -> [Rotation] -> [Rotation]
normalizeAndAdd dir amt res =
    let amount = abs amt `mod` 360
        amount' = if amount == 0 then 360 else amount
        rotation = if dir then CW amount' else CCW amount'
    in rotation : res


-- TODO: Question 3: define the 'unlockSequence' function
unlockSequence :: Int -> [Int] -> [Rotation]
unlockSequence currentState password = unlockSequenceHelper currentState password True

unlockSequenceHelper :: Int -> [Int] -> Bool -> [Rotation]
unlockSequenceHelper _ [] _ = []
unlockSequenceHelper currentState (targetState:rest) isCW =
    let amount = computeRotationAmount currentState targetState isCW
        rotation = if isCW then CW amount else CCW amount
        nextState = (if isCW then currentState + amount else currentState - amount) `mod` 360
    in rotation : unlockSequenceHelper nextState rest (not isCW)

computeRotationAmount :: Int -> Int -> Bool -> Int
computeRotationAmount currentState targetState isCW =
    let rawAmount = if isCW
            then (targetState - currentState) `mod` 360
            else (currentState - targetState) `mod` 360
        amount = if rawAmount == 0 then 360 else rawAmount
    in amount


-- TODO: Question 4: define the 'willOpen' function
willOpen = undefined