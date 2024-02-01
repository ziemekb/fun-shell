import System.IO
import System.Directory
import System.FilePath
import System.Posix.Signals
import Data.Either
import Command
import Lexer
import Jobs

prompt :: IO ()
prompt = do
    cwd <- getCurrentDirectory
    let dir = takeBaseName cwd
    putStr dir
    putStr " #> "

eval :: (Command, Background) -> IO () 
eval (cmd, bg)
    | isLeft cmd = launchJob (fromLeft [] cmd, bg)
    | otherwise  = launchPipeline (fromRight [] cmd, bg) 

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
            --putStrLn input
            let tokens = tokenize input
            _ <- eval tokens 
            --putStrLn $ showList tokens ""
            readLoop

main :: IO ()
main = do 
    hSetBuffering stdin LineBuffering
    tdevin  <- hIsTerminalDevice stdin
    tdevout <- hIsTerminalDevice stdout
    let mask = emptySignalSet
    _ <- installHandler sigTSTP Ignore $ Just mask
    _ <- installHandler sigTTOU Ignore $ Just mask
    _ <- installHandler sigTTIN Ignore $ Just mask
    _ <- installHandler sigINT  Ignore $ Just mask
    if tdevin && tdevout 
        then readLoop 
        else return ()
