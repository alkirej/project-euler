
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
    putStrLn "Working on euler problem #0027."
    putStrLn ""

    constr <- Cmd.numFromCommandLine

    let (num,a,b) = findMostStartingPrimes constr
        answer    = a * b

    putStr "The answer to the problem with |a| and |b| <= "
    putStr $ show constr
    putStr " is "
    putStr $ show answer
    putStrLn ".  It is the product of"
    putStr "        "
    putStr $ show a
    putStr " and "
    putStr $ show b
    putStr ".  n^2 + "
    putStr $ show a
    putStr "n + "
    putStr $ show b
    putStr " creates primes when n=0.."
    putStr $ show num
    putStrLn "."

findMostStartingPrimes :: Int -> (Int,Int,Int)
findMostStartingPrimes max
    | max < 1 = error $ "Invalid max parameter in findMostStartingPrimes ("
                    ++ show max ++ ")"
findMostStartingPrimes max =
        let abPairs = [(a',b') | a'<-[-max..max], b'<-[-max..max]]
            all     = map getStartingPrimes abPairs
        in  Lst.maximum all

calculate :: Int -> Int -> Int -> Int
calculate n a b = n^2 + a*n + b

getStartingPrimes :: (Int,Int) -> (Int,Int,Int)
getStartingPrimes (a,b) =
        if Ftor.isPrime b then
            getStartingPrimes' 0 (a,b)
        else
            (0,0,0)

getStartingPrimes' :: Int -> (Int,Int) -> (Int,Int,Int)
getStartingPrimes' found (a,b)  =
    if Ftor.isPrime $ calculate found a b then
        getStartingPrimes' (found+1) (a,b)
    else
        (found-1,a,b)


getStartingPrimes'2 :: [Int] -> Int -> (Int,Int) -> ((Int,Int,Int),[Int])
getStartingPrimes'2 sofar found (a,b)  =
    let val = calculate found a b
    in  if Ftor.isPrime val then
            getStartingPrimes'2 (val:sofar) (found+1) (a,b)
        else
            ((found-1,a,b),sofar)

-- -999 and 61