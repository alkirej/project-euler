
import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor
import qualified MathFns                        as Mfns


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0014."
    putStrLn ""

    mblt <- Cmd.numFromCommandLine  -- Starting number must be lower than

    putStrLn $ "Finding the longest Collatz sequence ending in 1 with a "
                    ++ " starting value under " ++ show mblt

    let (ans,seq) = loop (mblt-1) (1,1,[1])
    putStrLn ""
    putStrLn $ "The answer is " ++ show ans ++ " with a sequence "
                    ++ show (length seq) ++ " elements long."

loop :: Int -> (Int,Int,[Int]) -> (Int,[Int])
-- n = the current value to get the sequence for
loop 1 (nbr,len,lst) = (nbr,lst)
loop n mx@(nbr,len,lst) =
    let new = Mfns.collatzSeq n
    in  if length new > len then
            loop (n-1) (n,length new,new)
        else
            loop (n-1) mx