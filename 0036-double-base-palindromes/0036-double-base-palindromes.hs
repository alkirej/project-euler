
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf
import qualified MathFns                        as Mthf
import qualified StringFns                      as Strf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0036."
    putStrLn ""

    upTo <- Cmd.numFromCommandLine

    let doubles = doublePalindrome upTo
    print doubles
    print $ sum doubles

doublePalindrome :: Int -> [Int]
doublePalindrome = doublePalindrome' []

doublePalindrome' :: [Int] -> Int -> [Int]
doublePalindrome' sofar 0 = sofar
doublePalindrome' sofar n =
    let isDecPal = Mthf.isNumericPalindrome n
        isBinPal = Strf.isPalindrome $ Mthf.toBase 2 n
    in  if isDecPal && isBinPal then
            doublePalindrome' (n:sofar) (n-1)
        else
            doublePalindrome' sofar (n-1)