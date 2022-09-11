
import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0010."
    putStrLn ""

    st <- Cmd.numFromCommandLine -- # to stop looking for primes at.

    let prs = findAllPrimes st
        s   = sum prs
    putStr "    There are "
    putStr $ show $ length prs
    putStr " prime numbers under "
    putStr $ show st
    putStrLn "."
    putStrLn ""

    putStr   "    Their sum is "
    putStr   $ show s
    putStrLn "."
findAllPrimes :: Int -> [Int]
findAllPrimes n = filter Ftor.isPrime [2..n-1]