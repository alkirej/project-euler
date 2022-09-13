
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified ConversionFns                  as Conv
import qualified Factor                         as Ftor
import qualified FileFns                        as Filf
import qualified ListFns                        as Lstf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0021."
    putStrLn ""

    fn <- Cmd.textFromCommandLine

    conts <- Filf.readWholeFile fn

    let names = Txt.splitOn "," conts
        names' = Lst.sort $ map (Txt.dropAround ('"'==)) names
        scs = map Conv.scoreName names'
        scs' = zipWith3 (\x y z->(x,y,z)) [1..] scs names'
        ans = sum $ map (\(idx,scr,_)->idx*scr) scs'

    putStr "The sum of the name scores * the sorted index = "
    putStr $ show ans
    putStrLn "."