module MatrixFns
--    (   listsStartingAt
--
--    )
where

import qualified Data.Matrix                    as Mtx
import           Data.Matrix( (!) )

allListsOfLen :: Mtx.Matrix Int -> Int -> [ [Int] ]
allListsOfLen m ln =
        let (w,l) = (Mtx.ncols m, Mtx.nrows m)            -- matrix size
            sps   = [ (x,y) | x <- [1..w],y <- [1..l] ]   -- starting points
            als   = concatMap (legalLines m ln) sps  -- all lines
        in  linesOfVals m als

linesOfVals :: Mtx.Matrix Int -> [ [(Int,Int)] ] -> [ [Int] ]
linesOfVals m cs = linesOfVals' m cs []

linesOfVals' :: Mtx.Matrix Int -> [ [(Int,Int)] ] -> [ [Int] ]
             -> [ [Int] ]
linesOfVals' _ [] ps = ps
linesOfVals' m (l:ls) ps =
        let vals = coordsToVals m l
        in  linesOfVals' m ls (vals:ps)

coordsToVals :: Mtx.Matrix Int -> [(Int,Int)] -> [Int]
coordsToVals m cs = coordsToVals' m cs []

coordsToVals' :: Mtx.Matrix Int -> [(Int,Int)] -> [Int]
              -> [Int]
-- m  = matrix
-- c  = coordinates
-- ls = remaining coordinates in list
-- ps = progress
coordsToVals' _ [] ps = ps
coordsToVals' m (c:cs) ps =
        coordsToVals' m cs (m!c:ps)

legalLines :: Mtx.Matrix Int -> Int -> (Int,Int)
           -> [ [(Int,Int)] ]
legalLines m l (x,y) =
    let lns = allLines l (x,y)
    in  filter (filterLines m) lns

allLines :: Int -> (Int,Int) -> [ [(Int,Int)] ]
-- get the coords for one line (without worrying about edges)
-- l       = length of the line
-- ( x, y) = starting coords
-- (dx,dy) = line direction
allLines l (x,y) =
    -- searching all 8 directions leads to duplication of work.
    let dirs = [(0,1),(1,0),(1,1),(1,-1)]
    in  map (buildLine l (x,y) []) dirs

-- get the coords for one line (without worrying about edges)
-- l       = length of the line
-- ( x, y) = starting coords
-- (dx,dy) = line direction (deltas for direction)
-- ps      = progress
buildLine  :: Int -> (Int,Int) -> [(Int,Int)] -> (Int,Int)
           -> [(Int,Int)]
buildLine 0 _  ps _ = ps
buildLine _ _ ps (0,0) = ps
buildLine l (x,y) ps (dx,dy) =
      --     1 less|new "start"|deltas |new progress
    buildLine (l-1) (x+dx,y+dy) ((x,y):ps) (dx,dy)

filterLines :: Mtx.Matrix Int -> [ (Int,Int) ] -> Bool
-- m     = matrix
-- cs    = coordinate pairs
filterLines m cs =
    let (w,l) = (Mtx.ncols m, Mtx.nrows m)
    in  all (\(x,y) -> x>=1 && y >= 1 && x<=l && y<= w) cs

