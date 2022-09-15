module CmdLine
    (   numFromCommandLine,
        numFromCommandLineArg,
        textFromCommandLine
    )
where

import qualified Data.Char                      as Ch
import qualified Data.Text                      as Txt
import qualified System.Environment             as Env

verifyNumeric :: String -> Bool
verifyNumeric []       = True
verifyNumeric (ch:str) = foldr ((&&) . Ch.isDigit) True str

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

numFromCommandLine :: IO Int
numFromCommandLine = do
    args <- Env.getArgs
    return $ numericCommandLineArgument args 1

numFromCommandLineArg :: Int -> IO Int
numFromCommandLineArg idx = do
    args <- Env.getArgs
    return $ numericCommandLineArgument args idx


textFromCommandLine :: IO Txt.Text
textFromCommandLine = do
    args <- Env.getArgs
    if null args then
        error "    No command line arguments found."
    else
        return $ Txt.pack $ head args
