module Parser.ReadOnlyParser.CommandROP where
import Control.Applicative
import Parser.Core
import Parser.ReadOnlyParser.AexpROP(aexp)
import Parser.ReadOnlyParser.BexpROP(bexp)
import qualified Debug.Trace as DBG

program :: Parser String
program = 
    do
        x <- command
        y <- program
        return (x ++ y)
    <|> 
    command

command :: Parser String
command = 
    assignment 
    <|>
    ifThenElse
    <|>
    while
    <|>
    do
        symbol "skip"
        symbol ";"

assignment :: Parser String
assignment = 
    do
        var <- identifier
        symbol "="
        val <- P(\env input ->
            let took = takeWhile (/=';') input
            in Just (env, took, drop (length took) input))
        symbol ";"
        return (var ++ "=" ++ val ++ ";")
    <|>
    do
        var <- identifier
        symbol "["
        i <- aexp
        symbol "]"
        symbol "="
        val <- P(\env input ->
            let took = takeWhile (/=';') input
            in Just (env, took, drop (length took) input))
        symbol ";"
        return (var ++ "["++i++"]=" ++ val ++ ";")
    <|>
    do
        var <- identifier
        symbol "["
        ir <- aexp
        symbol "]"
        symbol "["
        ic <- aexp
        symbol "]"
        symbol "="
        val <- P(\env input ->
            let took = takeWhile (/=';') input
            in Just (env, took, drop (length took) input))
        symbol ";"
        return (var ++ "["++(show ir)++"]"++"["++(show ic)++"]=" ++ val ++ ";")

ifThenElse :: Parser String
ifThenElse = 
    do {
        symbol "if";
        symbol "(";
        b <- bexp;
        symbol ")";
        symbol "{";
        ifBody <- program;
        symbol "}";
        do
            symbol "else"
            symbol "{"
            elseBody <- program
            symbol "}"
            return ("if (" ++ b ++ ") {" ++ ifBody ++ "}" ++ " else {" ++ elseBody ++ "}")
        <|>
        return ("if (" ++ b ++ ") {" ++ ifBody ++ "}");
    }

while :: Parser String
while =
    do {
        symbol "while";
        symbol "(";
        b <- bexp;
        symbol ")";
        symbol "{";
        x <- program;
        symbol "}";
        --DBG.traceM("while (" ++ b ++ ") {" ++ x ++ "}");
        return ("while (" ++ b ++ ") {" ++ x ++ "}");
    }

repeatWhileString :: String -> Parser String
repeatWhileString s = P(\env input -> Just (env, "", s ++ input))
