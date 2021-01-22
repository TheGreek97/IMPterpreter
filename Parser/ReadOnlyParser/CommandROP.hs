module Parser.ReadOnlyParser.CommandROP where
import Control.Applicative
import Parser.Core
import Parser.ReadOnlyParser.AexpROP(aexp)
import Parser.ReadOnlyParser.BexpROP(bexp)

program :: Parser String
program = 
    do
        x <- command
        symbol ";"
        y <- program
        return (x ++ ";" ++ y)
    <|> 
    do 
        x <- command
        symbol ";"
        return (x ++ ";")
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
    symbol "skip"

assignment :: Parser String
assignment = 
    do
        var <- identifier
        symbol "="
        val <- P(\env input ->
            let took = takeWhile (/=';') input
            in Just (env, took, drop (length took) input))
        return (var ++ "=" ++ val)

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
            return ("if " ++ b ++ " {" ++ ifBody ++ "}" ++ " else {" ++ elseBody ++ "}")
        <|>
        return ("if " ++ b ++ " {" ++ ifBody ++ "}");
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
        return ("while (" ++ b ++ ") {" ++ x ++ "}");
    }

repeatWhileString :: String -> Parser String
repeatWhileString s = P(\env input -> Just (env, "", s ++ input))
