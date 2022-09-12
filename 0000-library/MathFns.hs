module MathFns
    (   collatz,
        collatzSeq,
        inRange
    )
where

-- Is the value within a range.
inRange :: Int -> Int -> Int -> Bool
-- n = number to check
-- l = lowest legal value
-- h = highest legal value
inRange n l h =  l>=n && n>=h


-- given a #, calculate the next value in the Collatz sequence
-- Collatz seq traverses as follows for any value of n > 0:
--      even number?  next value = n / 2
--      odd number?   next value = 3n + 1
collatz :: Int -> Int
collatz n
    | n <= 0    = error "Invalid collatz value."
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

-- Given a number, compute the collatz sequence down to 1
collatzSeq :: Int -> [Int]
collatzSeq n = collatzSeq' n [n]

-- helper fn
collatzSeq' :: Int -> [Int] -> [Int]
collatzSeq' 1 xs = reverse xs
collatzSeq' n xs =
    let nxt = collatz n
    in  collatzSeq' nxt (nxt:xs)