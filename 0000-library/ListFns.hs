module ListFns
    (   containsAll
    )
where

import qualified Data.List                      as Lst

-- ********************************************************************
-- CONTAINS ALL
--      does one list contain all the elements of the other?
-- ********************************************************************
-- are all members of xs in ys?
containsAll :: Eq a => [a] -> [a] -> Bool
containsAll xs = all (`elem` xs)

