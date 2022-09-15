module ListFns
    (   binarySearch,
        binSrchContains,
        containsAll,
        integerToDigitList,
        numericStringToDigitList,
        numericTextToDigitList,
        readNumbersToLists,
        swapTuples,
        wrap
    )
where

import qualified Data.Char                      as Ch
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl
import qualified Data.Maybe                     as Mb
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

binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch = binarySearch' 0

binarySearch' :: (Ord a) => Int -> [a] -> a -> Maybe Int
-- list = list to search
-- val  = value to search for
-- idx  = current index within list
binarySearch' _ [] _ = Nothing
binarySearch' idx list val
    | val == head list = Just idx
    | len == 1         = Nothing   -- only candidate is incorrect.
    | val < head ys    = binarySearch' idx xs val
    | otherwise        = binarySearch' (idx+half) ys val
    where
        len     = length list
        half    = len `div` 2
        (xs,ys) = splitAt half list

binSrchContains :: (Ord a) => [a] -> a -> Bool
binSrchContains list val = Mb.isJust $ binarySearch list val

-- ******************************************************************** --
-- LOOK FOR REPEATED CONSECUTIVE ELEMENTS IN THE LIST.
-- ******************************************************************** --
-- lst = the complete list to search
-- seg = the segment to find
-- loc = current index. (To start at front of list, call with 0).
-- return Nothing if the segment is not found and an Just indx
--          with the index of seg in lst if found.
matches :: Eq a => [a] -> [a] -> Int -> Maybe Int
matches [] _ _ = Nothing
matches lst@(_:rest) seg loc =
    let segLn = length seg
        lstSeg = take segLn lst
    in  if lstSeg == seg then
            Just loc
        else
            matches rest seg (loc+1)

matchFront :: Eq a => [a] -> [a] -> Bool
matchFront lst seg = Just 0 == matches lst seg 0

repeatAtFrontCount :: Eq a => [a] -> [a] -> Int
repeatAtFrontCount = repeatAtFrontCount' 0

repeatAtFrontCount' :: Eq a => Int -> [a] -> [a] -> Int
repeatAtFrontCount' ct [] _ = ct
repeatAtFrontCount' ct _ [] = ct
repeatAtFrontCount' ct lst seg =
    if matchFront lst seg then
        repeatAtFrontCount' (ct+1) rest seg
    else
        ct
    where (frt,rest) = splitAt (length seg) lst

-- Does this repeated segment cover at least 1/2 of the list?
isRepeatedSegment :: Eq a => [a] -> [a] -> Bool
isRepeatedSegment lst seg =
    let ct = repeatAtFrontCount lst seg
        ln = ct * length seg
        half = length lst `div` 2
    in  ln > half

lookForRepeatingSegmentInFront :: Eq a => [a] -> [a]
lookForRepeatingSegmentInFront lst =
        reverse $ lookForRepeatingSegmentInFront' 1 (length lst `div` 3) lst []

lookForRepeatingSegmentInFront' :: Eq a => Int -> Int -> [a] -> [a] -> [a]
lookForRepeatingSegmentInFront' crt mx lst best
    | crt > mx  = best
    | otherwise = do
            let seg = take crt lst

            if isRepeatedSegment seg best then
                    best
            else if isRepeatedSegment lst seg then
                    lookForRepeatingSegmentInFront' (crt+1) mx lst seg
            else
                    lookForRepeatingSegmentInFront' (crt+1) mx lst best

-- ---------------------------------------------------------
-- Wrap the first element of a list to the end
--   Example:  "hello"
--   becomes:  "elloh"
-- ---------------------------------------------------------
wrap :: [a] -> [a]
wrap []   = []
wrap (x:xs) = xs ++ [x]

