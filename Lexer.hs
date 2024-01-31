module Lexer where

data Token = NULL | AND | OR | PIPE | BGJOB | COMMAND [String]
    deriving (Show)

tokenize :: String -> [Token]
tokenize line =
    strToTok (words line) []

strToTok :: [String] -> [String] -> [Token]
strToTok [] []            = []
strToTok [] acc           = [COMMAND (reverse acc)]
strToTok ("&&": args) []  = AND   : strToTok args []
strToTok ("||": args) []  = OR    : strToTok args []
strToTok ("|" : args) []  = PIPE  : strToTok args []
strToTok ("&" : args) []  = BGJOB : strToTok args []
strToTok ("&&": args) acc = COMMAND (reverse acc) : AND   : strToTok args []
strToTok ("||": args) acc = COMMAND (reverse acc) : OR    : strToTok args []
strToTok ("|" : args) acc = COMMAND (reverse acc) : PIPE  : strToTok args []
strToTok ("&" : args) acc = COMMAND (reverse acc) : BGJOB : strToTok args []
strToTok (cmd : args) acc = strToTok args $ cmd : acc
