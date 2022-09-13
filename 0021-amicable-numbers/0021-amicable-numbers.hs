
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0021."
    putStrLn ""

    mx <- Cmd.numFromCommandLine

    let pds = allProperDivisorSums (mx-1)  -- proper divisors
        swps = Lstf.swapTuples pds     -- swap tuples to look for amicables
        -- amicables will be in both lists (and not be equal).
        -- > removes x==y and duplicats simultaneously.
        -- amis = filter (\(x,y) -> x>y) $ Lst.intersect swps pds
        amis = filter (uncurry (>)) (swps `Lst.intersect` pds)
        ttl  = sum $ map (uncurry (+)) amis

    putStr "The sum of all amicable numbers under "
    putStr $ show mx
    putStr " is "
    print ttl

allProperDivisorSums :: Int -> [(Int,Int)]
allProperDivisorSums n = allProperDivisorSums' n []

allProperDivisorSums' :: Int -> [(Int,Int)] -> [(Int,Int)]
allProperDivisorSums' 0 sofar = sofar
allProperDivisorSums' n sofar =
        let pdsm = Ftor.sumOfProperDivisors n
        in  allProperDivisorSums' (n-1) ((n,pdsm):sofar)