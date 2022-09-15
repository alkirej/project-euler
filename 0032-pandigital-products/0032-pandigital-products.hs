
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf
import qualified MathFns                        as Mthf

digits :: String
digits = "123456789"

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0032."
    putStrLn ""

    let perms1    = Lst.permutations digits
        poss1     = map strToNbrs1 perms1
        results1  = filter (\(x,y,z)->x*y==z) poss1
        results1' = map ( \(x,y,z)->z ) results1

    let perms2    = Lst.permutations digits
        poss2     = map strToNbrs2 perms2
        results2  = filter (\(x,y,z)->x*y==z) poss2
        results2' = map ( \(x,y,z)->z ) results2

        answer   = sum $ Lst.nub (results1' ++ results2')

    print answer

strToNbrs1 :: String -> (Int,Int,Int)
strToNbrs1 str =
    let digits = Lstf.numericStringToDigitList str
    in  (  (digits!!0) *   10 + (digits!!1),
           (digits!!2) *  100 + (digits!!3) *  10 + (digits!!4),
           (digits!!5) * 1000 + (digits!!6) * 100 + (digits!!7) * 10
                        + (digits!!8)
        )

strToNbrs2 :: String -> (Int,Int,Int)
strToNbrs2 str =
    let digits = Lstf.numericStringToDigitList str
    in  (  (digits!!0),
           (digits!!1) * 1000 + (digits!!2) *  100 + (digits!!3) *  10
                        + (digits!!4),
           (digits!!5) * 1000 + (digits!!6) * 100 + (digits!!7) * 10
                        + (digits!!8)
        )