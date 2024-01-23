import Control.Monad
import System.IO
import Data.Char

data Token = NULL | AND | OR | PIPE | BGJOB | CMD String
    deriving (Show)

prompt :: IO ()
prompt = putStr "#> "

tokenMapping :: String -> Token
tokenMapping "&&" = AND
tokenMapping "||" = OR
tokenMapping "|"  = PIPE
tokenMapping "&"  = BGJOB
tokenMapping cmd  = CMD cmd

tokenize :: String -> IO ([Token])
tokenize str = do
                let tokens = words str
                return $ map tokenMapping tokens

readLoop :: IO ()
readLoop = do
        prompt
        eof <- isEOF
        if eof 
            then do
                putStrLn ""
                return () 
            else do 
                input <- getLine
                putStrLn input
                tokens <- tokenize input
                putStrLn $ showList tokens ""
                readLoop

main :: IO ()
main = do 
        hSetBuffering stdin LineBuffering
        tdevin  <- hIsTerminalDevice stdin
        tdevout <- hIsTerminalDevice stdout
        if tdevin && tdevout 
            then readLoop 
            else return ()
