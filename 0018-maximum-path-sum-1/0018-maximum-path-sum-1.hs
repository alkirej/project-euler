
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0018."
    putStrLn ""

    fileNm  <- Cmd.textFromCommandLine      -- filename of matrix

    tri <- Lstf.readNumbersToLists fileNm
    let tri' = reverse tri

    let answer = combineRows tri'
    putStrLn $ "The maximum path through the supplied triangle is : "
                    ++ show answer

combineRows :: [[Int]] -> Int
combineRows [] = 0
combineRows (r:rs) = combineRows' r rs

combineRows' :: [Int] -> [[Int]] -> Int
combineRows' [last] [] = last
combineRows' last (r:rs) =
    let pr = processRow last
        cr = combineRow r pr
    in  combineRows' cr rs
combineRows' last rows =
    let x = error $ show last
        y = error $ show rows
    in error $ "\n" ++ show last ++ "\n" ++ show rows

combineRow :: [Int] -> [Int] -> [Int]
combineRow nxtRow mxPath =
    zipWith (+) nxtRow mxPath

processRow :: [Int] -> [Int]
processRow rw = reverse $ processRow' rw []

processRow' :: [Int] -> [Int] -> [Int]
processRow' []  ps      = ps
processRow' [n] ps      = ps
processRow' (a:b:ns) ps =
    let pth = if a > b then a else b -- total on the path so far.
    in  processRow' (b:ns) (pth:ps)