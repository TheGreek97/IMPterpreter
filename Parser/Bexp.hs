module Parser.Bexp where
import Control.Applicative
import Parser.Core
import Parser.Aexp(aexp)
import Parser.EnvironmentManager

bexp :: Parser Bool
bexp = 
    do  p1 <- bterm
        symbol "||"
        p2 <- bexp
        return (p1 || p2)
    <|>
    do  p1 <- bterm
        symbol "&&"
        p2 <- bexp
        return (p1 && p2)
    <|>
    bcompare
    <|>
    do bterm

bterm :: Parser Bool
bterm = 
    do  
        symbol "!"
        b <- bterm
        return $ not b
    <|> 
    do  
        symbol "("
        b <- bexp
        symbol ")"
        return b
    <|>
    do  
        symbol "true"
        return True
    <|>
    do 
        symbol "false"
        return False
    <|>
    bcompare
    <|>
    do
        i <- identifier
        var <- readVariable i
        case var of
            Left var -> return (var == 0) -- If the integer value is != 0 it is consider as False
            Right var -> empty

bcompare :: Parser Bool
bcompare =
    do
        a <- aexp
        symbol "<"
        b <- aexp
        return (a < b)
    <|>
    do
        a <- aexp
        symbol "<="
        b <- aexp
        return (a <= b)
    <|>
    do
        a <- aexp
        symbol ">"
        b <- aexp
        return (a > b)
    <|>
    do 
        a <- aexp
        symbol ">="
        b <- aexp
        return (a >= b)
    <|>
    do
        a <- aexp
        symbol "=="
        b <- aexp
        return (a == b)
    <|>
    do 
        a <- aexp
        symbol "!="
        b <- aexp
        return (a /= b)