import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt

import qualified CmdLine                        as Cmd
import qualified ConversionFns                  as Conv
import qualified Factor                         as Ftor


main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn "Working on euler problem #0017."
    putStrLn ""

    mx <- Cmd.numFromCommandLine  -- Count the letters in words up to.

    let wrds = loop mx
        chrs = map (Txt.filter Ch.isLetter) wrds  -- letter count
        lc   = sum $ map Txt.length chrs

        msg  = "I counted "
                   `Txt.append` Txt.pack (show lc)
                    `Txt.append` " letters in the words needed to count to "
                    `Txt.append` Txt.pack (show mx)

    putStrLn $ Txt.unpack msg


loop :: Int -> [Txt.Text]
loop n = loop' n []

loop' :: Int -> [Txt.Text] -> [Txt.Text]
loop' 0 ns = ns
loop' n ns =
        let word = Conv.numberToWord n
        in  loop' (n-1) (word:ns)