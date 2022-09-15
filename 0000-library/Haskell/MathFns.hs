module MathFns
    (   collatz,
        collatzSeq,
        decimalLongDivision,
        fibonacciSequenceUpTo,
        inRange,
        isNumericPalindrome,
        nextFib,
        toBase
    )
where

import qualified Data.Char                      as Ch

-- Is the value within a range.
inRange :: Int -> Int -> Int -> Bool
-- n = number to check
-- l = lowest legal value
-- h = highest legal value
inRange n l h =  l>=n && n>=h

-- Is this # a palindrone?
isNumericPalindrome :: Int -> Bool
isNumericPalindrome n =
    let f = show n
        b = reverse f
    in f == b

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


-- ******************************************************************** --
-- FIBONACCI SEQUENCE FNS
-- ******************************************************************** --
fibStart :: [Integer]
fibStart = [2,1,1]

fibonacciSequenceUpTo :: Integer -> [Integer]
fibonacciSequenceUpTo = fibsUnder fibStart

fibsUnder :: [Integer] -> Integer -> [Integer]
fibsUnder fibs mx =
    let next = nextFibFromList fibs
    in  if next > mx then
            fibs
        else
            fibsUnder (nextFibFromList fibs:fibs) mx

nextFibFromList :: [Integer] -> Integer
nextFibFromList (a:b:rest) = a + b

nextFib :: Integer -> Integer -> Integer
nextFib a b = a + b

decimalLongDivision :: Int -> Int -> Int -> String
decimalLongDivision plcs num den = reverse $ longDivision "" plcs num den

longDivision :: String -> Int -> Int -> Int -> String
longDivision sofar 0 _ _ = sofar
longDivision sofar plcs num den =
        let (val,rmd) = intDiv num den
            newStr = if null sofar then
                        "." ++ show val
                    else
                        Ch.intToDigit val:sofar
        in longDivision newStr (plcs-1) (10*rmd) den

intDiv :: Int -> Int -> (Int,Int)
intDiv x y = (x `div` y, x `mod` y)

toBase :: Int -> Int -> String
toBase base decimal
    | base <  2 = error $ "Invalid base (" ++ show base ++ ")"
    | base > 10 = error "Bases above 10 not implemented yet."
    | otherwise = toBase' "" base decimal

toBase' :: String -> Int -> Int -> String
toBase' sofar _ 0 = sofar
toBase' sofar base decimal =
    let rem = decimal `mod` base
        nxt = decimal `div` base
        res = show rem ++ sofar
    in  toBase' res base nxt
