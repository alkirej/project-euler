

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0006."
    putStrLn ""

    let ns  = [1..100]
        sosq = sumOfSquares  ns
        sqts = squareTheSum  ns

    putStr   "    The sum of the squares is: "
    putStrLn $ show sosq
    putStr   "    The square of the sums is: "
    putStrLn $ show sqts
    putStrLn ""
    putStr   "    The difference is: "
    putStrLn $ show (sqts-sosq)

sumOfSquares :: [Int] -> Int
sumOfSquares []     = 0
sumOfSquares (n:ns) = (n*n) + sumOfSquares ns

squareTheSum :: [Int] -> Int
squareTheSum ns =
    let s = sum ns
    in  s * s