
prompt :: IO ()
prompt = putStr "#> "

-- Here lexical tokenization takes place
parse :: String -> [t]
parse s = []

-- Here interpretation of the token or some abstract syntax
interpret :: [t] -> ()
interpret a = ()

main :: IO ()
main = do prompt
          input <- getLine
          main
