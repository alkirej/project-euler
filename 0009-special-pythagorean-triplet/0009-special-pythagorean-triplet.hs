
import qualified Control.Monad                  as Ctrl
import qualified Data.Maybe                     as Mb
import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0009."
    putStrLn ""

    s <- Cmd.numFromCommandLine  -- a + b + c = s

    let x   = [1..(s-2)]        -- nums up to desired sum
        ans = [ (a,b,c) | a <- x, b <- x, c <- x,
                        c > a, c > b, b > a,
                        s == a+b+c,
                        a^2 + b^2 == c^2 ]
        prd = firstProduct ans

    putStrLn "    Finding all Pythagorean triplets made of natural numbers "
    putStr   "    that add to "
    putStr   $ show s
    putStrLn "."
    putStrLn ""
    putStrLn "    This means that "
    putStrLn "        - a,b, and c are positive whole numbers, "
    putStrLn "        - a^2 + b^2 = c^2, AND "
    putStr   "        - a + b + c = "
    print s
    putStrLn ""

    if null ans then do
        putStrLn "    I COULD FIND NO MATCHES!"
    else do
        putStr   "    HERE ARE THE MATCHES I FOUND: "
        print ans

    Ctrl.when (Mb.isJust prd)
        ( do
            let result = Mb.fromJust prd
            putStrLn ""
            putStrLn "    The euler problem requires the product of the three numbers:"
            putStr   "        IT IS ----   "
            putStr   $ show result
            putStrLn "   ---"
        )

firstProduct :: [(Int,Int,Int)] -> Mb.Maybe Int
firstProduct []          = Mb.Nothing
firstProduct ((a,b,c):_) = Mb.Just $ a * b * c