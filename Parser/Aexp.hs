module Parser.Aexp where
import Control.Applicative
import Parser.Core
import Parser.EnvironmentManager (readVariable)

aexp :: Parser Int
aexp = 
    do
        t <- aterm
        symbol "+"
        a <- aexp 
        return (t+a)
    <|>
    do
        t <- aterm
        symbol "-"
        a <- aexp
        return (t-a)
    <|>
    aterm

aterm :: Parser Int
aterm = 
    do
        f <- afactor
        symbol "*"
        t <- aterm
        return (f*t)
    <|>
    do
        f <- afactor
        symbol "/"
        t <- aterm
        if (t == 0) then empty
        else return (f `div` t) 
    <|>
    afactor

afactor :: Parser Int
afactor = 
    do 
        symbol "("
        a <- aexp
        symbol ")"
        return a
    <|>
    do
        i <- identifier
        readVariable i
    <|>
    integer
