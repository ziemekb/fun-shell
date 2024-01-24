import System.Directory
import System.Environment
import System.Posix.Process

data Builtin = CD | PWD -- | EXIT | PWD | KILL | BG | FG | JOBS
    deriving (Show)

cmdMapping :: Builtin -> ([String] -> IO Int)
cmdMapping CD   = cd
-- cmdMapping EXIT = exit
cmdMapping PWD = pwd
-- cmdMapping KILL = kill

-- TO DO: ERROR HANDLING
-- TO DO: ADD ARGUMENTS HANDLING

cd :: [String] -> IO Int
cd (filepath:args) = do
                    setCurrentDirectory filepath  
                    return 0
    
pwd :: [String] -> IO Int 
pwd args = do 
        path <- getCurrentDirectory
        putStrLn path
        return 0

execute_external :: [String] -> IO ([String])
execute_external (cmd:args) = do
    if elem '/' cmd
        then executeFile cmd False args Nothing
        else executeFile cmd True args Nothing

