module Jobs where
import System.Posix.Signals
import System.Posix.Process
import System.Posix.Terminal
import System.Posix.IO
import System.Posix.Types
import System.Posix.Files
import GHC.IO.Handle.FD
import Data.Maybe
import Data.Monoid
import Data.Either
import Command
import Lexer

waitForTerminal :: IO ()
waitForTerminal = do
    ttypgid <- getTerminalProcessGroupID 0
    pgid    <- getProcessGroupID
    if ttypgid /= pgid
        then waitForTerminal
        else return ()

doInputRedir :: (Maybe FilePath) -> IO (Maybe Fd)
doInputRedir Nothing = return Nothing
doInputRedir (Just input) = do 
    inputFd <- openFd input 
                      ReadOnly 
                      (Just stdFileMode)
                      defaultFileFlags
    return $ Just inputFd
    --dupCloseFd (Just inputFd) stdInput 

doOutputRedir :: (Maybe FilePath) -> IO (Maybe Fd)
doOutputRedir Nothing = return Nothing
doOutputRedir (Just output) = do 
    outputFd <- openFd output 
                       ReadWrite 
                       (Just stdFileMode)
                       defaultFileFlags
    return $ Just outputFd
    --dupCloseFd (Just outputFd) stdOutput 

launchJob :: (Command, Background) -> IO ()
launchJob (command, bg) 
    | isBuiltin singleCmd = do
        exitcode <- executeBuiltin singleCmd
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
            inputFd  <- doInputRedir  inputPath
            outputFd <- doOutputRedir outputPath
            dupCloseFd inputFd  stdInput
            dupCloseFd outputFd stdOutput
            executeExternal singleCmd
        createProcessGroupFor id
        setTerminalProcessGroupID 0 id
        -- wait for process
        status <- getProcessStatus True True id
        pgid <- getProcessGroupID
        -- get back control of the terminal
        setTerminalProcessGroupID 0 pgid
        return ()
    where singleCmd  = fromLeft [] $ cmd command
          inputPath  = input command
          outputPath = output command

-- temporary solution for waiting for children without job control
waitForChildren :: Int -> IO ()
waitForChildren n
    | n == 0    = return ()
    | otherwise = do 
        getProcessStatus True True (-1)
        waitForChildren $ n - 1

launchPipeline :: (Command, Background) -> IO ()
launchPipeline (command, bg) = do 
    firstInputFd <- doInputRedir  $ input  command
    lastOutputFd <- doOutputRedir $ output command
    pipelineLoop 0 firstInputFd lastOutputFd $ fromRight [] $ cmd command  
    waitForChildren $ length $ fromRight [] $ cmd command
    pgid <- getProcessGroupID
    setTerminalProcessGroupID 0 pgid
    return ()

pipelineLoop :: ProcessGroupID -> (Maybe Fd) -> (Maybe Fd) -> Pipeline -> IO ()
pipelineLoop pgid (Just fd) lastOutput (command:[]) = do 
    pipeProcess pgid ((Just fd), lastOutput) command 
    closeFd fd
    return ()
pipelineLoop pgid input lastOutput (command:rest) = do
    (nextInput, output) <- createPipe
    newPgid <- pipeProcess pgid (input, (Just output)) command
    case input of
        Just fd -> closeFd fd
        Nothing -> return ()
    closeFd output
    pipelineLoop newPgid (Just nextInput) lastOutput rest

dupCloseFd :: (Maybe Fd) -> Fd -> IO ()
dupCloseFd (Just fd1) fd2 = dupTo fd1 fd2 >> closeFd fd1 
dupCloseFd Nothing    fd2 = return ()

pipeProcess :: ProcessGroupID -> (Maybe Fd, Maybe Fd) -> SingleCommand -> IO (ProcessGroupID)
pipeProcess pgid (fdIn, fdOut) cmd = do
    id <- forkProcess $ do
        joinProcessGroup pgid
        let saMask = emptySignalSet
        _ <- installHandler sigTSTP Default $ Just saMask
        _ <- installHandler sigTTOU Default $ Just saMask
        _ <- installHandler sigTTIN Default $ Just saMask
        _ <- installHandler sigINT  Default $ Just saMask
        _ <- waitForTerminal
        dupCloseFd fdIn  stdInput
        dupCloseFd fdOut stdOutput
        executeExternal cmd
    let newPgid = if pgid == 0 then id else pgid
    setProcessGroupIDOf id pgid
    setTerminalProcessGroupID stdInput newPgid
    return newPgid

