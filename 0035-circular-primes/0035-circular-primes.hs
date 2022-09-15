
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
    putStrLn "Working on euler problem #0035."
    putStrLn ""

    stopAt <- Cmd.numFromCommandLine

    let answers = filter check [1..stopAt]

    print answers
    print $ length answers

check :: Int -> Bool
check n =
    let digCt = length $ show n
    in  not ('0' `elem` (show n)) && check' digCt n

        --if '0' `elem` (show n) then
        --    False
        --else
        --    check' digCt n

check' :: Int -> Int -> Bool
check' 0 n = True
check' digCnt n =
    if not $ Ftor.isPrime n then
        False
    else
        check' (digCnt-1) nextNbr
    where crtStr  = show n
          nextStr = Lstf.wrap crtStr
          nextNbr = read nextStr :: Int
