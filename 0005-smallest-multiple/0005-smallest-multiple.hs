
-- import Factor
import qualified Data.List                      as Lst

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0005."
    putStrLn ""

    print $ findSmallestDivisibleByAll [1..10]

-- find the smallest int divisible by all values if fs.
findSmallestDivisibleByAll :: [Int] -> Int
findSmallestDivisibleByAll fs =
    let mxd = maximum fs  -- highest divisor in list
    in  findSmallestDivisibleByAll' fs mxd mxd

findSmallestDivisibleByAll' :: [Int] -> Int -> Int -> Int
findSmallestDivisibleByAll' fs n step =
    if isDivisibleByAll fs n then
        n
    else findSmallestDivisibleByAll' fs (n+step) step



-- is x divisible by all numbers in ns?
isDivisibleByAll :: [Int] -> Int -> Bool
isDivisibleByAll ns x =
    let fs = factor x
    in  includesAll fs ns

-- are all members of xs in ys?
includesAll :: Eq a => [a] -> [a] -> Bool
includesAll xs = all (`elem` xs)

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

isPrime :: Int -> Bool
isPrime p = null (factor p)
