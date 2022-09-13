
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified DateFns                        as Datf
import qualified Factor                         as Ftor


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0019."
    putStrLn ""

    let start = (1901,01,01)
        end   = (2000,12,31)
        sunDf = Datf.daysUntilSunday $ Datf.dayOfWeek start
        start' = (1901,01,01+sunDf)

        allSd = sundays start' end []
        fsts  = filter (\(y,m,d) -> d==1) allSd

    putStrLn $ "During the 20th century, Sunday fell on the 1st of the month "
                    ++ show (length fsts) ++ " times."


sundays :: Datf.SimpleDate -> Datf.SimpleDate -> [Datf.SimpleDate]
        -> [Datf.SimpleDate]
sundays next last lst =
    if next > last then
        lst
    else
        sundays (Datf.addSevenDays next) last (next:lst)