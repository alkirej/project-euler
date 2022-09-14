
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
    putStrLn "Working on euler problem #0028."
    putStrLn ""

    sz <- Cmd.numFromCommandLine

    let crnrs  = allCorners sz
        answer = sum crnrs

    putStr "The sum of all corners in a "
    putStr $ show sz
    putStr "x"
    putStr $ show sz
    putStr " matrix as described in euler problem #28 is "
    putStr $ show answer
    putStrLn "."

allCorners :: Int -> [Int]
allCorners = allCorners' [] (1,0)

allCorners' :: [Int] -> (Int,Int) -> Int -> [Int]
allCorners' sofar (crnt,lst) max =
    let new = matrixCorners lst crnt
    in  if crnt > max then
            sofar
        else
            allCorners' (sofar++new) (crnt+2,Lst.last new) max

matrixCorners :: Int -> Int -> [Int]
matrixCorners _ 1 = [1]
matrixCorners endLst rows
    | even rows   =  error "Invalid rows entry in matrix corners."
    | even endLst =  error "Invalid rows entry in matrix corners."
    | otherwise   =  let incr = (rows-1)
                     in  nextInSeq 4 endLst incr


nextInSeq :: Int -> Int -> Int -> [Int]
nextInSeq = nextInSeq' []

nextInSeq' :: [Int] -> Int -> Int -> Int -> [Int]
nextInSeq' sofar 0 _ _ = sofar
nextInSeq' sofar count start incr =
    let next = start + incr
    in  nextInSeq' (sofar++[next]) (count-1) (start+incr) incr