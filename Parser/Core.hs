module Parser.Core where
import Environment (Variable, Env)
import Control.Applicative
import Data.Char

--Returns a Maybe value because the parsing can fail
newtype Parser a = P (Env -> String -> Maybe(Env, a, String))

parse :: Parser a -> Env -> String -> Maybe(Env, a,String)
parse (P p) env input = p env input

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\env input -> case parse p env input of 
        Nothing -> Nothing
        Just (env, v, out) -> Just (env, g v, out))

instance Applicative Parser where
    --pure :: a -> Parser a
    pure v = P (\env input -> Just (env, v, input))
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\env input -> case parse pg env input of
        Nothing  -> Nothing
        Just (env, g, out) -> parse (fmap g px) env out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    return v = P (\env input -> Just (env, v, input)) -- Has the same behaviour as "pure"

    p >>= f = P (\env input -> case parse p env input of
        Nothing -> Nothing
        Just (env, v, out) -> parse (f v) env out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\env input -> Nothing)

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\env input -> case parse p env input of
        Nothing -> parse q env input
        Just (env, v, out) -> Just (env, v, out))

--Fundamental Types

item :: Parser Char
item = P (\env input -> case input of 
    [] -> Nothing
    (x:xs) -> Just (env,x,xs))

sat :: (Char -> Bool) -> Parser Char
sat p = 
    do  x <- item
        if p x then return x else empty
 
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)
 
string :: String -> Parser String
string [] = return []
string (x:xs) = 
    do
        char x
        string xs
        return (x:xs)

identifier :: Parser String
identifier = 
    do
        x <- lower
        xs <- many alphanum
        return (x:xs)

nat :: Parser Int
nat = 
    do
        xs <- some digit
        return (read xs)

space :: Parser ()
space = 
    do 
        many (sat isSpace)
        return ()
 
int :: Parser Int
int = 
    do
        char '-'
        n <- nat
        return (-n)
    <|>
    nat
 
token :: Parser a -> Parser a
token p = 
    do
        space
        v <- p
        space
        return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)