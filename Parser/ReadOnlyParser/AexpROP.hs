module Parser.ReadOnlyParser.AexpROP where
import Control.Applicative
import Parser.Core
import qualified Debug.Trace as DBG

aexp :: Parser String
aexp = 
    do 
       at <- aterm
       symbol "+"
       ax <- aexp
       return (at ++ "+" ++ ax)
    <|>
    do 
        at <- aterm
        symbol "-"
        ax <- aexp
        return (at ++ "-" ++ ax)
    <|>
    aterm

aterm :: Parser String
aterm = 
    do 
        af <- afactor
        symbol "*"
        at <- aterm
        return (af ++ "*" ++ at)
    <|> 
    afactor

afactor :: Parser String
afactor = 
    do
        symbol "("
        ax <- aexp
        symbol ")"
        return ("(" ++ ax ++ ")")
    <|>
    do 
        varName <- identifier
        symbol "["
        ax <- aexp
        symbol "]"
        return (varName ++ "[" ++ ax ++ "]")
    <|>
    identifier
    <|>
    do 
        x <- integer
        return ("" ++ (show x))