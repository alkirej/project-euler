main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    let even = filter (\n -> n `mod` 2 == 0) fib
    putStrLn "Even #s from Fibonacci sequence under 4 million: "
    putStr   "        "
    putStrLn $ show even
    putStrLn ""
    putStr "Answer: "
    putStrLn $ show $ sum even


fib :: [Int]
fib = fibs [2,1]

fibs :: [Int] -> [Int]
fibs seq =
    let next = nextFib seq
    in  if next > 4_000_000 then
            seq
        else
            fibs (nextFib seq:seq)

nextFib :: [Int] -> Int
nextFib (a:b:rest) = a + b
