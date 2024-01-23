import System.Directory

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

