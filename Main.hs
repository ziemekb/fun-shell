import Control.Monad
import System.IO
import Data.Char
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import qualified GHC.IO.FD as FDOps
import Foreign.C.Types
import Command

data Token = NULL | AND | OR | PIPE | BGJOB | WORD String
    deriving (Show)

type Command  = [String]
type Exitcode = Int

prompt :: IO ()
prompt = putStr "#> "

tokenMapping :: String -> Token
tokenMapping "&&" = AND
tokenMapping "||" = OR
tokenMapping "|"  = PIPE
tokenMapping "&"  = BGJOB
tokenMapping word = WORD word 

tokenize :: String -> IO ([Token])
tokenize str = do
    let tokens = words str
    return $ map tokenMapping tokens

waitForTerminal :: IO ()
waitForTerminal = do
    ttypgid <- getTerminalProcessGroupID 0
    pgid    <- getProcessGroupID
    if ttypgid /= pgid
        then waitForTerminal
        else return ()
    
launchJob :: [String] -> IO ()
launchJob cmd = do
    {--
     - necessary to get back control of terminal
    let mask = emptySignalSet
    _ <- installHandler sigTTOU Ignore $ Just mask
    _ <- installHandler sigTTIN Ignore $ Just mask
    --}
    --command <- cmd 
    id <- forkProcess $ do
        createProcessGroupFor 0 
        let saMask = emptySignalSet
        _ <- installHandler sigTSTP Default $ Just saMask
        _ <- installHandler sigTTOU Default $ Just saMask
        _ <- installHandler sigTTIN Default $ Just saMask
        _ <- installHandler sigINT  Default $ Just saMask
        _ <- installHandler sigQUIT Default $ Just saMask
        _ <- installHandler sigCHLD Default $ Just saMask
        _ <- waitForTerminal 
        executeExternal cmd --command
    let stdinFD = FDOps.fdFD FDOps.stdin -- file descriptor of standard input
    createProcessGroupFor id
    setTerminalProcessGroupID 0 id -- stdinFD id
    -- wait for job 
    status <- getProcessStatus True True id
    pgid <- getProcessGroupID
    -- get back control of the terminal
    setTerminalProcessGroupID 0 pgid -- stdinFD pgid
    -- putStrLn "Done"
    return () 

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
    let mask = emptySignalSet
    _ <- installHandler sigTSTP Ignore $ Just mask
    _ <- installHandler sigTTOU Ignore $ Just mask
    _ <- installHandler sigTTIN Ignore $ Just mask
    _ <- installHandler sigINT  Ignore $ Just mask
    _ <- installHandler sigQUIT Ignore $ Just mask
    _ <- installHandler sigCHLD Ignore $ Just mask
    if tdevin && tdevout 
        then readLoop 
        else return ()


