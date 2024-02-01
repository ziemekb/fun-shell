module Jobs where
import System.Posix.Signals
import System.Posix.Process
import System.Posix.Terminal
import System.Posix.IO
import System.Posix.Types
import Data.Maybe
import Data.Monoid
import Command
import Lexer

waitForTerminal :: IO ()
waitForTerminal = do
    ttypgid <- getTerminalProcessGroupID 0
    pgid    <- getProcessGroupID
    if ttypgid /= pgid
        then waitForTerminal
        else return ()

launchJob :: (SingleCommand, Background) -> IO ()
launchJob (cmd, bg) 
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
        createProcessGroupFor id
        setTerminalProcessGroupID 0 id
        -- wait for process
        status <- getProcessStatus True True id
        pgid <- getProcessGroupID
        -- get back control of the terminal
        setTerminalProcessGroupID 0 pgid
        return ()

{--
launchPipeline :: (Pipeline, Background) -> IO ()
launchPipeline (pipe, bg) = do
    (nextInput, output) <- createPipeFd
    pipeProcess pgid input output
--}

pipeFd :: (Maybe Fd) -> Fd -> IO ()
pipeFd (Just fd1) fd2 = dupTo fd1 fd2 >> closeFd fd1 
pipeFd Nothing    fd2 = return ()

pipeProcess :: ProcessGroupID -> (Maybe Fd, Maybe Fd) -> SingleCommand -> IO ()
pipeProcess pgid (fdIn, fdOut) cmd = do
    id <- forkProcess $ do
        joinProcessGroup pgid
        let saMask = emptySignalSet
        _ <- installHandler sigTSTP Default $ Just saMask
        _ <- installHandler sigTTOU Default $ Just saMask
        _ <- installHandler sigTTIN Default $ Just saMask
        _ <- installHandler sigINT  Default $ Just saMask
        _ <- waitForTerminal
        pipeFd fdIn  stdInput
        pipeFd fdOut stdOutput
        executeExternal cmd
    setProcessGroupIDOf id pgid
    setTerminalProcessGroupID stdInput pgid
    return ()
        




