

import qualified Data.List.Split                as Spl
import qualified Data.Matrix                    as Mtx
import qualified Data.Text                      as Txt

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified MatrixFns                      as MxFn


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0011."
    putStrLn ""

    fileNm  <- Cmd.textFromCommandLine      -- filename of matrix
    lnLen   <- Cmd.numFromCommandLineArg 2  -- length of list

    contents <- readFile $ Txt.unpack fileNm
    let ls  = Spl.splitOn "\n" contents  -- split into lines
        cs  = map (Spl.splitOn " ") ls   -- split into cells
        cs' = concat cs
        ns  = map (\str -> read str :: Int) cs'
        w   = length $ head cs
        l   = length ls

    putStr   "    Reading "
    putStr $ Txt.unpack fileNm
    putStr   ".  It is "
    putStr $ show $ length contents
    putStr " characters and "
    putStr $ show $ length ls
    putStrLn " lines long."

    let mx = Mtx.fromList w l ns
        vs = MxFn.allListsOfLen mx lnLen -- valid lines
        (ans,ansList) = maxProd vs (0,[])

    putStrLn ""
    putStr   "    I found this list "
    putStr   $ show ansList
    putStr   " whos product is "
    putStr   $ show ans
    putStrLn "."

maxProd :: [ [Int] ] -> (Int,[Int]) -> (Int,[Int])
-- l  = line being processed
-- ls = unprocessed lines so far
-- p  = best so far (progress)
maxProd [] best = best
maxProd (l:ls) (mx,mxl) =
    let res = product l
    in  if res > mx then
            maxProd ls (res,l)
        else
            maxProd ls (mx,mxl)