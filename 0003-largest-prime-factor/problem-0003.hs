import Control.Monad

main :: IO ()
main = do
    let number = 600851475143
        fs = factor number
        ps = filter isPrime fs

    replicateM_ 5 $ putStrLn ""
    putStr "Factors of "
    putStr $ show number
    putStr ":  "
    putStrLn $ show fs
    putStr "Prime Factors:            "
    putStrLn $ show ps
    putStr "Answer:                   "
    putStrLn $ show $ last ps


factor :: Int -> [Int]
factor i = reverse $ findFactorsOf i 2 []

-- Int   - number to factor
-- Int   - highest number considered
-- [Int] - list of factors found
findFactorsOf :: Int -> Int -> [Int] -> [Int]
findFactorsOf n c found =
    let done = c*c > n
    in  if done then
            found
        else if isFactor n c then
            findFactorsOf n (c+1) (c:found)
        else
            findFactorsOf n (c+1) found

isFactor :: Int -> Int -> Bool
isFactor n f = (n `mod` f == 0)

isPrime :: Int -> Bool
isPrime p = null (factor p)
