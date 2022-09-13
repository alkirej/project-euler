module FileFns
    (   readWholeFile
    )
where

import qualified Data.Text                      as Txt

readWholeFile :: Txt.Text -> IO Txt.Text
readWholeFile fn = do
    contents <- readFile $ Txt.unpack fn
    return $ Txt.pack contents