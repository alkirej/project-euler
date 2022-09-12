
import qualified Data.List.Split                as Spl
import qualified Data.Text                      as Txt

import qualified CmdLine                        as Cmd
import qualified Factor                         as Ftor


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0013."
    putStrLn ""

    fileNm  <- Cmd.textFromCommandLine      -- filename of 50 digit #s.
    contents <- readFile $ Txt.unpack fileNm

    let ls  = Spl.splitOn "\n" contents  -- split into lines
        ns  = map (\str -> read str :: Integer ) ls
        s   = sum ns
    putStr "The sum of the numbers in the file is: "
    putStrLn $ show s
