module Parser.ReadOnlyParser.BexpROP where
import Control.Applicative
import Parser.Core
import Parser.ReadOnlyParser.AexpROP(aexp)

bexp :: Parser String
bexp = 
    do 
        a <- bterm
        symbol "||"
        b <- bexp
        return (a ++ "||" ++ b)
    <|>
    bterm

bterm :: Parser String
bterm = 
    do 
        a <- bfactor
        symbol "&&"
        b <- bterm
        return (a ++ "&&" ++ b)
    <|>
    bfactor

bfactor :: Parser String
bfactor = 
    do 
        symbol "!"
        b <- bfactor
        return ("!" ++ b)
    <|>
    do 
        symbol "("
        b <- bexp
        symbol ")"
        return ("(" ++ b ++ ")")
    <|>
    symbol "true"
    <|>
    symbol "false"
    <|> 
    identifier
    <|> 
    bcomparison

bcomparison :: Parser String
bcomparison = 
    do
        a <- aexp
        symbol "=="
        b <- aexp
        return (a ++ "==" ++ b)
    <|> 
    do
        a <- aexp
        symbol "<="
        b <- aexp
        return (a ++ "<=" ++ b)
    <|> 
    do
        a <- aexp
        symbol "<"
        b <- aexp
        return (a ++ "<" ++ b)
    <|> 
    do
        a <- aexp
        symbol ">="
        b <- aexp
        return (a ++ ">=" ++ b)
    <|> 
    do
        a <- aexp
        symbol ">"
        b <- aexp
        return (a ++ ">" ++ b)