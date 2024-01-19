import Control.Monad
import System.IO
import Data.Char

prompt :: IO ()
prompt = putStr "#> "

{--
-- Here lexical tokenization takes place
parse :: String -> [t]
parse s = concatMap 

-- Here interpretation of the token or some abstract syntax
interpret :: [t] -> ()
interpret a = ()
--}

readLine :: IO (String)
readLine = 
        do 
            input <- getLine
            return $ map (\c -> if isSpace c then '\0' else c) input

mainLoop :: IO ()
mainLoop = do
        prompt
        eof <- isEOF
        if eof 
            then return () 
            else do 
                input <- readLine
                putStrLn input
                mainLoop

main :: IO ()
main = do 
        hSetBuffering stdin LineBuffering
        tdevin  <- hIsTerminalDevice stdin
        tdevout <- hIsTerminalDevice stdout
        if tdevin && tdevout then mainLoop else return ()
