import System.IO
import System.Directory
import System.FilePath
import System.Posix.Signals
import Command
import Lexer
import Jobs

prompt :: IO ()
prompt = do
    cwd <- getCurrentDirectory
    let dir = takeBaseName cwd
    putStr dir
    putStr " #> "

eval :: [Token] -> IO () 
eval tokens 
    | bg == True = launchJob (head $ reverse $ tail revToks) bg 
    | otherwise  = launchJob (head $ reverse revToks) bg
    where revToks = reverse tokens
          bg      = head revToks == BG

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
