
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
    putStrLn "Working on euler problem #0034."
    putStrLn ""

    let answers = filter isValid [10..3628800]

    print $ sum answers

isValid :: Int -> Bool
isValid n =
    let digits = Lstf.numericStringToDigitList $ show n
        fctSum = sum $ map (\x->product [1..x]) digits
    in  n == fctSum

