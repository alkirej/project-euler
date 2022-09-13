
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf

allSummableAbove :: Int
allSummableAbove = 28123

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0023."
    putStrLn ""

    let anl  = abundantNumList allSummableAbove
        sums = Lst.sort $ filter (<=allSummableAbove)
                        $ Lst.nub
                        $ Lst.sort
                        $ [x+y|x<-anl,y<-anl,x>=y]

        all   = [1..allSummableAbove]
        allSm = sum all
        sumSm = sum sums
        diff  = allSm - sumSm

        --unSums = [x | x<-all, not $ x `elem` sums]
        --answer = sum unSums

    putStrLn ""
    putStr "The sum of all terms that cannot be expressed as the sum of two "
    putStr "abundant numbers is "
    --putStr $ show answer
    putStr $ show diff
    putStrLn "."

abundantNumList :: Int -> [Int]
abundantNumList mx = abundantNumList' mx []

abundantNumList' :: Int -> [Int] -> [Int]
abundantNumList' 0 found = found
abundantNumList' n found =
    if Ftor.isAbundantNumber n then
        abundantNumList' (n-1) (n:found)
    else
        abundantNumList' (n-1) found
