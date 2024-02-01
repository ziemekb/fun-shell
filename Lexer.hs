module Lexer where
import Data.List (groupBy)

type Background    = Bool
type SingleCommand = [String]
type Pipeline      = [SingleCommand]
type Command       = Either SingleCommand Pipeline

splitByDelimiter :: Eq a => a -> [a] -> [[a]]
splitByDelimiter delimiter = filter (not . null) . groupBy (\x y -> y /= delimiter) 

removeDelimiter :: Eq a => a -> [[a]] -> [[a]]
removeDelimiter delimiter = map (filter (/= delimiter))

splitAndRemoveDelimiter :: Eq a => a -> [a] -> [[a]]
splitAndRemoveDelimiter delimiter = removeDelimiter delimiter . splitByDelimiter delimiter

