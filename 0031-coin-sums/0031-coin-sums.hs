
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl
import qualified Data.Map                       as Map

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf
import qualified MathFns                        as Mthf

valids :: [Int]
valids = [1,2,5,10,20,50,100,200]

goal :: Int
goal = 200

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0031."
    putStrLn ""

    let answer = buildCoinSets (reverse valids) goal

    print $ length answer

-- Denoms must be in decreasing order.
buildCoinSets :: [Int] -> Int -> [ [(Int,Int)] ]
buildCoinSets [denom] amount =
        let nmCoins = coinsFor' denom amount
        in  [ [(nmCoins,denom)] ]
buildCoinSets (denom:denoms) amount =
    let poss    = possibleCoinCountsFor denom amount
        remains = map (\x -> leftToGet amount [x]) poss
        sets    = map (buildCoinSets denoms) remains

    in  concat $ zipWith (zipper []) poss sets


zipper :: [ [(Int,Int)] ] -> (Int,Int) ->[ [(Int,Int)] ] -> [ [(Int,Int)] ]
zipper sofar _ [] = sofar
zipper sofar addMe (s:ss) = zipper ((addMe:s):sofar) addMe ss

possibleCoinCountsFor :: Int -> Int -> [(Int,Int)]
possibleCoinCountsFor denom amount =
        let maxCoin = amount `div` denom
        in  map (,denom) [0..maxCoin]

coinsValue :: [(Int,Int)] -> Int
coinsValue [] = 0
coinsValue ((nbr,denom):rest) = nbr*denom + coinsValue rest

leftToGet :: Int -> [(Int,Int)] -> Int
leftToGet total coins = total - coinsValue coins

coinsFor' :: Int -> Int -> Int
coinsFor' denom amount = amount `div` denom

coinsFor :: Int -> [Int] -> [(Int,Int)]
coinsFor denom amts = map ((denom,) . coinsFor' denom) amts