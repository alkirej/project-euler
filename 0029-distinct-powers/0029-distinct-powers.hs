
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
    putStrLn "Working on euler problem #0029."
    putStrLn ""


    start <- Cmd.numFromCommandLine
    stop  <- Cmd.numFromCommandLineArg 2

    let start' = fromIntegral start :: Integer
        stop'  = fromIntegral stop  :: Integer
        lst    = [start'..stop']
        reslts = [ x^y | x<-lst, y<-lst]
        answer = length $ Lst.nub reslts

    putStr "The distint values of the form a^b where a and b are between "
    putStr $ show start
    putStr " and "
    putStr $ show stop
    putStr " is "
    putStr $ show  answer
    putStrLn "."

