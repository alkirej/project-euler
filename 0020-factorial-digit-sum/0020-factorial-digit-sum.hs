
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txtf
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0020."
    putStrLn ""

    fact <- Cmd.numFromCommandLine

    let fact' = fromIntegral fact :: Integer
        x = product [1..fact'] :: Integer
        nbrs = Lstf.integerToDigitList x
        answer = sum nbrs

    putStrLn ""
    putStr "The sum of the digits in "
    putStr $ show fact
    putStr "! is "
    putStr $ show answer
    putStrLn "."
