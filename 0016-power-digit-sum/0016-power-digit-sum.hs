
import qualified Data.Char                      as Ch

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0016."
    putStrLn ""

    sz <- Cmd.numFromCommandLine  -- The power of two to sum digits of.

    let big  = 2 ^ sz
        str  = show big

        sumd = sum $ map Ch.digitToInt str

    putStrLn $ "The sum of the digits in 2^" ++ show sz
                    ++ " is " ++ show sumd ++ "."
