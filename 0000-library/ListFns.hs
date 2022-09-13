module ListFns
    (   containsAll,
        integerToDigitList,
        numericStringToDigitList,
        numericTextToDigitList,
        readNumbersToLists,
        swapTuples
    )
where

import qualified Data.Char                      as Ch
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl
import qualified Data.Text                      as Txt

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

numericStringToDigitList :: String -> [Int]
numericStringToDigitList =
    foldl (\lst ch -> Ch.digitToInt ch:lst) []

numericTextToDigitList :: Txt.Text -> [Int]
numericTextToDigitList t = numericStringToDigitList (Txt.unpack t)

integerToDigitList :: Integer -> [Int]
integerToDigitList bigNum =
        let str = show bigNum
        in  numericStringToDigitList str

-- ---------------------------------------------------------
-- Swap first and second elements of each 2-tuple in a list
-- ---------------------------------------------------------
swapTuples :: [(a,b)] -> [(b,a)]
swapTuples xs = [(y,x) | (x,y) <- xs ]
