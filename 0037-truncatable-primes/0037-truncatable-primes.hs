
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf
import qualified MathFns                        as Mthf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0037."
    putStrLn ""

    upTo <- Cmd.numFromCommandLine

    let truncs = checkForTruncatable upTo
    print truncs
    print $ length truncs
    print $ sum truncs

checkForTruncatable :: Int -> [Int]
checkForTruncatable = checkForTruncatable' []

checkForTruncatable' :: [Int] -> Int -> [Int]
checkForTruncatable' sofar 10 = sofar
checkForTruncatable' sofar n =
        if isTruncatablePrime n then
            checkForTruncatable' (n:sofar) (n-1)
        else
            checkForTruncatable' sofar (n-1)

isTruncatablePrime :: Int -> Bool
isTruncatablePrime n = all Ftor.isPrime $ buildMustBePrimeList n

buildMustBePrimeList :: Int -> [Int]
buildMustBePrimeList n = Lst.sort $ n:truncRight n ++ truncLeft n

truncRight :: Int -> [Int]
truncRight = truncRight' []

truncRight' :: [Int] -> Int -> [Int]
truncRight' sofar n =
    let truncd = n `div` 10
    in  if truncd == 0 then
            sofar
        else
            truncRight' (truncd:sofar) truncd

truncLeft :: Int -> [Int]
truncLeft n =
    let str    = show n
        strLn  = length str
        trStrs = map (`drop` str) [1..strLn-1]
    in  map (\x -> read x :: Int) trStrs

