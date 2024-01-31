module Command where
import System.Directory
import System.Environment
import System.Posix.Process
import System.Exit
import Lexer

data Builtin = CD | PWD | EXIT -- | KILL | BG | FG | JOBS
    deriving (Show)

cmdMapping :: Builtin -> ([String] -> IO Int)
cmdMapping CD   = cd
cmdMapping EXIT = exit
cmdMapping PWD  = pwd
-- cmdMapping KILL = kill

-- TO DO: ERROR HANDLING
-- TO DO: ADD ARGUMENTS HANDLING

isBuiltin :: Token -> Bool
isBuiltin (COMMAND cmd) = 
    elem (head cmd) ["cd", "pwd", "exit"]
isBuiltin _ = False

cd :: [String] -> IO Int
cd (filepath:args) = do
                    setCurrentDirectory filepath  
                    return 0

exit :: [String] -> IO Int 
exit args = exitSuccess
    
pwd :: [String] -> IO Int 
pwd args = do 
        path <- getCurrentDirectory
        putStrLn path
        return 0

executeExternal :: [String] -> IO ()
executeExternal (cmd:args) = do
    if elem '/' cmd
        then executeFile cmd False args Nothing
        else executeFile cmd True args Nothing

