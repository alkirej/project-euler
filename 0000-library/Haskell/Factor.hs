module Factor
    (   allFactors,
        countFactors,
        factor,
        isAbundantNumber,
        isDeficientNumber,
        isFactor,
        isPerfectNumber,
        isPrime,
        primeFactors,
        properDivisors,
        sumOfProperDivisors
    )
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
    let fs = factor n
    in  length fs

allFactors :: Int -> [Int]
allFactors n =
    let pfs = primeFactors n
        ls  = Comb.partitions pfs
    in  Lst.nub $ concatMap products ls

factor :: Int -> [Int]
factor n =
    let pfs = primeFactors (abs n)
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

-- ********************************************************************
-- PROPER DIVISORS
-- ********************************************************************
properDivisors :: Int -> [Int]
properDivisors n = Lst.delete n $ factor n

sumOfProperDivisors :: Int -> Int
sumOfProperDivisors = sum . properDivisors

-- Perfect number means the sum of the proper divisors = the number itself
isPerfectNumber :: Int -> Bool
isPerfectNumber n = n == sumOfProperDivisors n

-- A deficient number means the sum of the proper divisors < the number itself
isDeficientNumber :: Int -> Bool
isDeficientNumber n = sumOfProperDivisors n < n

-- A deficient number means the sum of the proper divisors > the number itself
isAbundantNumber :: Int -> Bool
isAbundantNumber n = sumOfProperDivisors n > n
