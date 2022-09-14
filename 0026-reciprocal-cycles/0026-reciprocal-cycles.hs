
import qualified Data.Char                      as Ch
import qualified Data.Decimal                   as Dec
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf
import qualified MathFns                        as Mthf


import Data.Fixed

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0026."
    putStrLn ""

    upTo <- Cmd.numFromCommandLine

    let (ans,str) = lengthOfRepeat upTo

    putStr "The value 1/"
    putStr $ show ans
    putStr " has a repeating fraction part of "
    putStr str
    putStr ".  This has a length of "
    putStr $ show $ length str
    putStrLn "."
    putStr "    This is the longest decimal repitition of all recipocals from "
    putStr "1/1 to 1/"
    putStr $ show upTo
    putStrLn "."


-- find the fraction (1/n where n is 0 to supplied number)
--      with the longest repeating
lengthOfRepeat :: Int -> (Int,[Char])
lengthOfRepeat = lengthOfRepeat' (0,[])

lengthOfRepeat' :: (Int,[Char]) -> Int -> (Int,[Char])
lengthOfRepeat' best 1 =  best
lengthOfRepeat' (bst,bsts) den =
        let rec   = Mthf.decimalLongDivision 10000 1 den
            nmbr  = drop 1 $ reverse $ drop 5 rec
            rc    = Lstf.lookForRepeatingSegmentInFront nmbr
            nwBst = if length rc > length bsts then (den,rc) else (bst,bsts)
        in  lengthOfRepeat' nwBst (den-1)

