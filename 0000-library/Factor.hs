module Factor
    (   factor,
        isPrime
    )
where

import qualified Data.List                      as Lst

-- ********************************************************************
-- FACTOR
-- ********************************************************************
factor :: Int -> [Int]
factor i = reverse $ findFactorsOf i 1 []

-- Int   - number to factor
-- Int   - highest number considered
-- [Int] - list of factors found

findFactorsOf :: Int -> Int -> [Int] -> [Int]
findFactorsOf n c found =
    let done = c*c > n
    in  if done then
            found
        else if isFactor n c then
            let d = n `div` c
            in  reverse $ Lst.sort $ findFactorsOf n (c+1) (c:d:found)
        else
            findFactorsOf n (c+1) found

isFactor :: Int -> Int -> Bool
isFactor n f = n `mod` f == 0

-- ********************************************************************
-- IS PRIME?
-- ********************************************************************
isPrime :: Int -> Bool
isPrime 1 = False
isPrime p = 2 == length (factor p)

