
import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified Data.List                      as Lst
import qualified Data.List.Split                as Spl

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified ListFns                        as Lstf
import qualified MathFns                        as Mthf


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0030."
    putStrLn ""

    power <- Cmd.numFromCommandLine

    let sm = computeNthPowerOfDigitsSum (1,4)
    print $ sum $ testFirstN' [] 10 10000000 5


testFirstN' :: [Int] -> Int -> Int -> Int -> [Int]
testFirstN' sofar crt max _
    | crt < 10  = error "Invalid crt parameter in computeFirstN."
    | crt > max = sofar
testFirstN' sofar crt max expnt =
    let sm = computeNthPowerOfDigitsSum (crt,expnt)
    in  if sm==crt then
            testFirstN' (crt:sofar) (crt+1) max expnt
        else
            testFirstN' sofar (crt+1) max expnt

computeNthPowerOfDigitsSum :: (Int,Int) -> Int
computeNthPowerOfDigitsSum = sum . computeNthPowerOfDigits

computeNthPowerOfDigits :: (Int,Int) -> [Int]
computeNthPowerOfDigits = computeNthPowerOfDigits' []

computeNthPowerOfDigits' :: [Int] -> (Int,Int) -> [Int]
computeNthPowerOfDigits' sofar (0,_) = sofar
computeNthPowerOfDigits' sofar (n,expnt) =
        computeNthPowerOfDigits' (digit^expnt:sofar) (rest,expnt)
        where (digit,rest) = (n `mod` 10,n `div` 10)
