
import qualified Control.Monad                  as Ctrl
import qualified Data.List                      as Lst
import qualified GHC.Float                      as Flt

import qualified CmdLine                        as Cmd
import qualified Combinatorics                  as Comb
import qualified Factor                         as Ftor



import qualified System.Environment             as Env -- !!!

cOutputEvernNTimes :: Int
cOutputEvernNTimes = 1_000_000

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0012."
    putStrLn ""

    nf <- Cmd.numFromCommandLine  -- # factors needed to exceed.

    putStr   "Searching for the first \"triangle number\" with over "
    putStr   $ show nf
    putStrLn " factors:"
    putStr   $ "I will show progress every " ++ show cOutputEvernNTimes
                    ++ " numbers I check."
    putStrLn ""

    ans <- findAnswer nf
    let afs = Lst.sort $ Ftor.allFactors ans

    putStrLn ""
    putStrLn ""
    putStrLn "*** FOUND IT ***"
    putStrLn $ "The answer is: " ++ show ans ++ " with "
                    ++ (show $ length afs) ++ " factors"
    putStrLn "Its factors are: "
    putStrLn $ show afs
    return ()

findAnswer :: Int -> IO Int
findAnswer nf = findAnswer' (1,1) nf 1

findAnswer' :: (Int,Int) -> Int -> Int -> IO Int
-- tn = triangle number (n, sum of first n natural numbers)
-- nf = number of factors we are looking for
-- ct = number of triangle numbers we have tested.
findAnswer' tn@(_,ttl) nf cnt = do
    let ftc = Ftor.countFactors ttl   -- factor count

    Ctrl.when (cnt `mod` cOutputEvernNTimes == 0) $
        print $ "Triangle number # " ++ show cnt ++ " has "
                    ++ show ftc ++ " factors."
    if ftc > nf then
        return ttl
    else
        findAnswer' (nextTriangleNum tn) nf (cnt+1)

triangleNum :: Int -> (Int,Int)
triangleNum nbr = (nbr, sum [1..nbr])

-- better performing way, BUT the int -> float -> int
--      introduces noise in the numbers (and incorrect
--      results).
triangleNum' nbr =
    let nb1 = nbr + 1
        nb2 = nbr `divide` 2
        nb3 = Flt.float2Int $ fromIntegral nb1  *  nb2

    in  (nbr, nb3)

nextTriangleNum :: (Int,Int) -> (Int,Int)
nextTriangleNum (nbr,ttl) = (nbr+1,ttl+nbr+1)

-- --------------------------------------------------------------------
-- Divide 2 values returning a float.
-- --------------------------------------------------------------------
divide :: Integral a => a -> a -> Float
divide x y = fromIntegral x / fromIntegral y
