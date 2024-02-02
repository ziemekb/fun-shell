module Lexer where
import Data.List (groupBy)
import Control.Applicative

type Background    = Bool
type SingleCommand = [String]
type Pipeline      = [SingleCommand]
data Command       = Command {cmd    :: Either SingleCommand Pipeline, 
                              input  :: Maybe FilePath,
                              output :: Maybe FilePath}
    deriving (Show)

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

stringAfterSep :: [String] -> String -> Maybe FilePath
stringAfterSep [] sep = Nothing
stringAfterSep redirs sep = 
    let tmp = dropWhile (/= sep) redirs in
    case tmp of 
        []     -> Nothing 
        (c:[]) -> Nothing
        (c:rest) -> Just $ head rest

redirectFiles :: [[String]] -> (Maybe FilePath, Maybe FilePath)
redirectFiles toks = 
    foldr (\x acc -> 
        (stringAfterSep x "<" <|> fst acc, stringAfterSep x ">" <|> snd acc)) 
        (Nothing, Nothing) 
        toks

parser :: [String] -> Command
parser words 
    | isPipeline words = let cmds = splitAndRemoveDelimiter "|" words in
                         let (input, output) = redirectFiles cmds in
                         Command (Right $ map dropRedirs cmds)
                                 input
                                 output
    | otherwise        = let (input, output) = redirectFiles [words] in 
                         Command (Left $ dropRedirs words) 
                                 input
                                 output   
    where dropRedirs = takeWhile (\s -> s /= "<" && s /= ">") 
