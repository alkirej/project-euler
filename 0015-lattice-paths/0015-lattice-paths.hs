
import qualified Control.Monad                  as Ctrl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0015."
    putStrLn ""

    sz <- Cmd.numFromCommandLine  -- Size of the square lattice.

    let lns = setupLattice
        (wt,lns') = getWeightAt (sz,sz) lns

    putStrLn $ "There are " ++ show wt ++ " paths accross a "
                    ++ show sz ++ " by " ++ show sz
                    ++ " lattice."

-- The lattice node triplet.
--      first 2 values are x,y coords in the lattice.
--      the 3rd value is the weight of the node
--              weight = # of lattice paths to finish line.
type LatticeNode = (Int,Int,Int)

setupLattice :: [LatticeNode]
setupLattice = [ (0,0,0), (1,0,1), (0,1,1) ]


getWeightAt :: (Int,Int) -> [LatticeNode] -> (Int,[LatticeNode])
getWeightAt (x,y) lns =
    let wt = filter (\(a,b,wt) -> x==a && y==b ) lns
        (wt',lns') =
                if null wt then
                    buildWeightAt (x,y) lns
                else
                    (thd_of_3 $ head wt, lns)
    in  (wt',lns')

buildWeightAt :: (Int,Int) -> [ LatticeNode ] -> (Int,[LatticeNode])
buildWeightAt (0,y) lns = ( 1, (0,y,1):lns )
buildWeightAt (x,0) lns = ( 1, (x,0,1):lns )
buildWeightAt (x,y) lns =
    let (rgt,lns')  = getWeightAt (x-1,y) lns
        (dwn,lns'') = getWeightAt (x,y-1) lns'
        wt          = rgt + dwn
    in  (wt,(x,y,wt):lns'')

-- ---------------------------------------------------------
-- Tuple helper functions for 3-Tuples
-- ---------------------------------------------------------
fst_of_3 :: (a,b,c) -> a
fst_of_3( x, _, _) = x

snd_of_3 :: (a,b,c) -> b
snd_of_3( _, x, _) = x

thd_of_3 :: (a,b,c) -> c
thd_of_3 (_,_,x) = x
