
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
    putStrLn "Working on euler problem #0025."
    putStrLn ""

    nmDigs <- Cmd.numFromCommandLine

    let fs = Mthf.fibonacciSequenceUpTo (10 ^ (nmDigs-1))
        count  = length fs
        answer = 1 + count

    --print fs
    --print $ 10^nmDigs
    --print $ length fs

    putStr "There are "
    putStr $ show count
    putStr " entries in the Fibonacci sequence that are below 10^"
    putStr $ show nmDigs
    putStrLn ","
    putStr "    So, the first entry with "
    putStr $ show nmDigs
    putStr " is "
    putStr $ show answer
    putStrLn "."
