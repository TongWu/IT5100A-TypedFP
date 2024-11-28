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
willOpen :: Int -> [Int] -> [Int] -> Bool
willOpen currentState password rotationsInts
    | null password = True  -- If the password is empty, the safe is always unlocked
    | otherwise =
        let rotationsList = toRotations rotationsInts
            simulated = simulateRotations currentState rotationsList
            n = length password
            lenRotations = length rotationsList
        in if lenRotations < n then False  -- Not enough rotations to unlock
            else
                let lastNRotsAndStates = drop (lenRotations - n) simulated
                in isUnlockSequence lastNRotsAndStates password

-- Simulate rotations and get the dial states
simulateRotations :: Int -> [Rotation] -> [(Rotation, Int)]
simulateRotations _ [] = []
simulateRotations currentState (rotation:rest) =
    let amount = rotationAmount rotation
        newState = (currentState + amount) `mod` 360
    in (rotation, newState) : simulateRotations newState rest

-- Get the amount of rotation considering the direction
rotationAmount :: Rotation -> Int
rotationAmount (CW amt) = amt
rotationAmount (CCW amt) = -amt

-- Check if the last N rotations form an unlocking sequence
isUnlockSequence :: [(Rotation, Int)] -> [Int] -> Bool
isUnlockSequence rotationsAndStates password =
    let directions = map (isCW . fst) rotationsAndStates
        dialStates = map snd rotationsAndStates
        directionsExpected = take (length password) (cycle [True, False])  -- Alternating starting from CW
    in directions == directionsExpected && dialStates == password

-- Determine if a rotation is clockwise
isCW :: Rotation -> Bool
isCW (CW _) = True
isCW (CCW _) = False