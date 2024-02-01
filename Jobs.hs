module Jobs where
import System.Posix.Signals
import System.Posix.Process
import System.Posix.Terminal
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
