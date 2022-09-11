
import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor

main :: IO ()
main = do
    count <- Cmd.numFromCommandLine
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0007."
    putStrLn ""

    print $ primeList count

primeList :: Int -> [Int]
primeList count = reverse $ primeList' count 2 []

primeList' :: Int -> Int -> [Int] -> [Int]
-- r  = remaining
-- c  = current # to check
-- ps = progress
primeList' 0 _ ps = ps
primeList' r c ps =
    if Ftor.isPrime c then
        primeList' (r-1) (c+1) (c:ps)
    else
        primeList' r (c+1) ps