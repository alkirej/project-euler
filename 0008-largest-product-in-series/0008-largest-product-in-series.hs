

import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified MathFns                        as Mfns


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0008."
    putStrLn ""

    fileNm  <- Cmd.textFromCommandLine
    lenProd <- Cmd.numFromCommandLineArg 2

    putStrLn $ "    Reading " ++ (Txt.unpack fileNm) ++
                    " from disk:"
    contents <- readFile $ Txt.unpack fileNm

    putStr     "    I found a file with a length of "
    putStr $ show $ length contents
    putStrLn   " characters."

    let cont = map (Ch.digitToInt) $ filter (/='\n') contents
    putStr     "    I cleaned out newlines leaving "
    putStr $ show $ length cont
    putStrLn   " characters."

    putStrLn ""

    putStr   "    Searching the list for the "
    putStr $ show lenProd
    putStrLn " consecutive numerals with the largest product."
    putStrLn ""

    let (ans,nums) = doProd cont lenProd (0,[])
    putStr   "    I found "
    putStr $ show nums
    putStr   " which has a product of "
    putStrLn $ show ans

textToInt :: [Char] -> [Int] -> [Int]
textToInt [] ps  = ps
textToInt (c:cs)  ps =
        let c' = Ch.digitToInt c
        in  textToInt cs (c':ps)

doProd :: [Int] -> Int -> (Int,[Int]) -> (Int,[Int])
-- all  = List of numbers (n:ns)
-- ln   = Multiply this many numbers of the list together.
-- mx   = Maximum product found so far.
-- return Value:  Largest product found
doProd [] _ mx = mx
doProd all@(n:ns) ln (mx,mxs) =
    let mm = take ln all    -- multiply me
        rs = product mm     -- result
    in  if rs > mx then
            doProd ns ln (rs, mm)
        else
            doProd ns ln (mx,mxs)