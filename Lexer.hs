module Lexer where
import Data.List (groupBy)

type Background    = Bool
type SingleCommand = [String]
type Pipeline      = [SingleCommand]
type Command       = Either SingleCommand Pipeline
-- type FilePath = String  
-- data Redirection = (Input FilePath) Command  (Output FilePath)

splitByDelimiter :: Eq a => a -> [a] -> [[a]]
splitByDelimiter delimiter = filter (not . null) . groupBy (\x y -> y /= delimiter) 

removeDelimiter :: Eq a => a -> [[a]] -> [[a]]
removeDelimiter delimiter = map (filter (/= delimiter))

splitAndRemoveDelimiter :: Eq a => a -> [a] -> [[a]]
splitAndRemoveDelimiter delimiter = removeDelimiter delimiter . splitByDelimiter delimiter

isPipeline :: [String] -> Bool
isPipeline = elem "|" 

tokenize :: String -> (Command, Background)
tokenize line 
    | bg        = (parser $ init toks, bg)
    | otherwise = (parser toks, bg)
    where toks = words line
          bg   = last toks == "&"

parser :: [String] -> Command
parser words 
    | isPipeline words = Right $ splitAndRemoveDelimiter "|" words
    | otherwise        = Left words   
