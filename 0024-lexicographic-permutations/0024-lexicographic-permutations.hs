
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0024."
    putStrLn ""

    digCnt <- Cmd.numFromCommandLineArg 1
    which  <- Cmd.numFromCommandLineArg 2


    let digits = [0..digCnt]
        perms = Lst.sort $ Lst.permutations digits
        ans = perms !! (which-1)

    putStr "Entry # "
    putStr $ show which
    putStr " in the ordered list of permutations of "
    putStr $ show digits
    putStr " is "
    putStr $ show ans
    putStrLn "."

