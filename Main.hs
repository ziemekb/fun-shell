import Control.Monad
import System.IO
import Data.Char
import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import qualified GHC.IO.FD as FDOps
import Foreign.C.Types
import Command
import Lexer

prompt :: IO ()
prompt = do
    cwd <- getCurrentDirectory
    let dir = takeBaseName cwd
    putStr dir
    putStr " #> "

waitForTerminal :: IO ()
waitForTerminal = do
    ttypgid <- getTerminalProcessGroupID 0
    pgid    <- getProcessGroupID
    if ttypgid /= pgid
        then waitForTerminal
        else return ()
    
launchJob :: Token -> Bool -> IO ()
launchJob (COMMAND cmd) bg 
    | isBuiltin cmd = do 
        exitcode <- executeBuiltin cmd
        return ()
    | otherwise     = do
        id <- forkProcess $ do
            createProcessGroupFor 0 
            let saMask = emptySignalSet
            _ <- installHandler sigTSTP Default $ Just saMask
            _ <- installHandler sigTTOU Default $ Just saMask
            _ <- installHandler sigTTIN Default $ Just saMask
            _ <- installHandler sigINT  Default $ Just saMask
            _ <- waitForTerminal 
            executeExternal cmd 
        let stdinFD = FDOps.fdFD FDOps.stdin -- file descriptor of standard input
        createProcessGroupFor id
        setTerminalProcessGroupID 0 id 
        -- wait for process
        status <- getProcessStatus True True id
        pgid <- getProcessGroupID
        -- get back control of the terminal
        setTerminalProcessGroupID 0 pgid 
        return () 
launchJob _ bg = return ()

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
            _ <- launchJob (head tokens) False
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
