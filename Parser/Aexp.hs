module Parser.Aexp where
import Control.Applicative
import Parser.Core
import Parser.EnvironmentManager (readVariable, readFullVariable)

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
        var <- readVariable i
        case var of
            Left var -> return var --var contains an integer
            Right var -> empty
    <|>
    do
        i <- identifier
        symbol "["
        index <- aexp
        symbol "]"
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_int
            then
            case var of
                Left var -> empty
                Right var -> return $ (read var :: [Int]) !! index
            else
                empty
    <|>
    do
        i <- identifier
        symbol "["
        rIndex <- aexp
        symbol "]"
        symbol "["
        cIndex <- aexp
        symbol "]"
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_arr_int
            then
            case var of
                Left var -> empty
                Right var -> return $ ((read var :: [[Int]]) !! rIndex) !! cIndex
            else
                empty
    <|> 
    integer