module Command where
import System.Directory
import System.Environment
import System.Posix.Process
import System.Exit
import Lexer

cmdMapping :: String -> ([String] -> IO Int)
cmdMapping "cd"   = cd
cmdMapping "exit" = exit
cmdMapping "pwd"  = pwd
-- cmdMapping "kill" = kill

-- TO DO: ERROR HANDLING
-- TO DO: ADD ARGUMENTS HANDLING

isBuiltin :: [String] -> Bool
isBuiltin (cmd:options)  = 
    elem cmd ["cd", "pwd", "exit"]
isBuiltin _ = False

cd :: [String] -> IO Int
cd (filepath:options) = do
                    setCurrentDirectory filepath  
                    return 0

exit :: [String] -> IO Int 
exit args = exitSuccess
    
pwd :: [String] -> IO Int 
pwd args = do 
        path <- getCurrentDirectory
        putStrLn path
        return 0

executeBuiltin :: Token -> IO Int
executeBuiltin (cmd:options) = 
    cmdMapping (head cmd) (tail cmd)
executeBuiltin _ = return (-1)

executeExternal :: [String] -> IO ()
executeExternal (cmd:options) = do
    if elem '/' cmd
        then executeFile cmd False options Nothing
        else executeFile cmd True  options Nothing
