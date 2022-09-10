

import qualified Data.List as Lst

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0004."
    putStrLn ""

    let toMult = [100..999]
    putStr "  Combine all possible values:      "
    let ps = map multiply [ (x,y) | x <- toMult, y <- toMult ]
    putStr $ show $ length ps
    putStrLn " found, starting with    "
    putStr "                "
    putStrLn $ show $ take 10 ps
    putStrLn ""

    putStr "  Filter out non-palindrome values:   "
    let fps = filter isPalindrome ps
    putStr $ show $ length fps
    putStrLn " found, starting with    "
    putStr "                "
    putStrLn $ show $ take 10 fps
    putStrLn ""

    putStr "  Sort and get highest:   "
    let srt     = Lst.sort fps
        highest = Lst.last srt
    putStrLn $ show highest

multiply :: (Int,Int) -> Int
multiply (a,b) = a*b

isPalindrome :: Int -> Bool
isPalindrome n =
    let f = show n
        b = reverse f
    in f == b