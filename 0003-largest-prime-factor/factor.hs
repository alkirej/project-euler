
import qualified Data.List                      as Lst
import qualified System.Environment             as Env

import Control.Monad

verifyNumeric :: String -> Bool
verifyNumeric []       = True
verifyNumeric (ch:str) =
        if ch >= '0' && ch <= '9' then
            verifyNumeric str
        else
            False

usage :: IO ()
usage = do
    putStrLn ""
    putStrLn "           factor <# to factor>"
    putStrLn ""

main :: IO ()
main = do
    putStrLn ""
    args <- Env.getArgs
    let cla = if null args then "" else head args

    if "" == cla then do
        putStrLn  "    No input found."
        usage
    else if not $ verifyNumeric cla then do
        putStrLn $ "    " ++ (show cla) ++ " is not a positive whole number."
        usage
    else do
        let x  = read cla :: Int
            fs = factor x
        putStrLn $ show fs


factor :: Int -> [Int]
factor i = reverse $ findFactorsOf i 2 []

-- Int   - number to factor
-- Int   - highest number considered
-- [Int] - list of factors found
findFactorsOf :: Int -> Int -> [Int] -> [Int]
findFactorsOf n c found =
    let done = c*c > n
    in  if done then
            found
        else if isFactor n c then
            let d = n `div` c
            in  reverse $ Lst.sort $ findFactorsOf n (c+1) (c:d:found)
        else
            findFactorsOf n (c+1) found

isFactor :: Int -> Int -> Bool
isFactor n f = (n `mod` f == 0)

isPrime :: Int -> Bool
isPrime p = null (factor p)
