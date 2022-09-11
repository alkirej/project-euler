module CmdLine
    (   numFromCommandLine,
        numericCommandLineArgument
    )
where

import System.Environment                       as Env

verifyNumeric :: String -> Bool
verifyNumeric []       = True
verifyNumeric (ch:str) =
        if ch >= '0' && ch <= '9' then
            verifyNumeric str
        else
            False

inRange :: Int -> Int -> Int -> Bool
-- n = number to check
-- l = lowest legal value
-- h = highest legal value
inRange n l h =  l>=n && n>=h

numFromCommandLine :: IO Int
numFromCommandLine = do
    args <- Env.getArgs
    return $ numericCommandLineArgument args 1

numericCommandLineArgument :: [String] -> Int -> Int
numericCommandLineArgument args n
    | n < 1           =
            error "Invalid command line argument index."
    | n > length args =
            error "Invalid command line argument index."
    | otherwise       =
        let str = args !! (n-1)
        in  if verifyNumeric str then
                read str :: Int
            else
                error $ str ++ " is not a valid whole number."