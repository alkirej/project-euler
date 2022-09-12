module Factor
--    (   factor,
--        isPrime
--    )
where

import qualified Data.List                      as Lst
import qualified Data.Map                       as Mp
import qualified GHC.Float                      as Flt
import qualified Combinatorics                  as Comb

-- ********************************************************************
-- FACTOR - EFFORT 2 USING PRIME FACTORS
-- ********************************************************************
countFactors :: Int -> Int
countFactors n =
    let fs = allFactors n
    in  length fs

allFactors :: Int -> [Int]
allFactors n =
    let pfs = primeFactors n
        ls  = Comb.partitions pfs
    in  Lst.nub $ concatMap products ls

-- given a pair of list of ints, return a 2 member list containing
--      the products of the numbers in each list.
products :: ([Int],[Int]) -> [Int]
products (xs,ys) = [product xs, product ys]

-- Note: every pair of factors f and f' where (f * f' = n) will have
--          the lowr of the pair be below the square root of n.
primeFactors :: Int -> [Int]
primeFactors n =                             -- n   = number to factor
    let mxf = Flt.float2Int (sqrt $ fromIntegral n)    -- mxf = maximum factor to consider
    in  Lst.sort $ findPrimeFactorsOf n mxf

-- n   = # to factor
-- c   = new # to consider as factor
-- fnd = factors found so far
findPrimeFactorsOf :: Int -> Int -> [Int]
findPrimeFactorsOf n c
    | c == 1    = [n]
    | n  < 4    = [n]
    | otherwise =
            if isFactor n c then do
                let nf = n `div` c         -- new factor
                primeFactors c ++ primeFactors nf
            else do
                findPrimeFactorsOf n (c-1)

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

-- ********************************************************************
-- IS f A FACTOR OF n?
-- ********************************************************************
isFactor :: Int -> Int -> Bool
isFactor n f = n `mod` f == 0

-- ********************************************************************
-- IS PRIME?
-- ********************************************************************
isPrime :: Int -> Bool
isPrime 1 = False
isPrime p = 2 == length (factor p)


