module ListFns
    (   containsAll,
        readNumbersToLists
    )
where

import qualified Data.List                      as Lst
import qualified Data.Text                      as Txt
import qualified Data.List.Split                as Spl

-- ********************************************************************
-- CONTAINS ALL
--      does one list contain all the elements of the other?
-- ********************************************************************
-- are all members of xs in ys?
containsAll :: Eq a => [a] -> [a] -> Bool
containsAll xs = all (`elem` xs)


readNumbersToLists :: Txt.Text -> IO [ [Int] ]
-- fn = file name
readNumbersToLists fn = do
    contents <- readFile $ Txt.unpack fn

    let ls  = Spl.splitOn "\n" contents  -- split into lines
        cs  = map (Spl.splitOn " ") ls   -- split into cells
    return $ map stringsToNumbers cs

listOfStrsToListOfNumbers :: [[String]] -> [[Int]]
listOfStrsToListOfNumbers = map stringsToNumbers

stringsToNumbers :: [String] -> [Int]
stringsToNumbers = map (\str -> read str :: Int)