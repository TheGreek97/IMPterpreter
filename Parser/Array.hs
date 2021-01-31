module Parser.Array where
import Control.Applicative
import qualified Debug.Trace as DBG
import Parser.Core
import Parser.Aexp (aexp)
import Parser.Bexp (bexp)
import Parser.EnvironmentManager

-- Arrays of Integers
aexpArray :: Parser [Int]
aexpArray =
    -- Scalar Multiplication
    do
        a <- arrayInt
        symbol "*"
        t <- aexp
        return (fmap (*t) a)
    <|>
    do
        t <- aexp
        symbol "*"
        a <- arrayInt
        return (fmap (*t) a)
    <|>
    -- Scalar Division
    do
        a <- arrayInt 
        symbol "/"
        t <- aexp
        if (t == 0) then empty
        else return (fmap (`div` t) a) 
    <|>
    -- Term-by-term operations
    do
        a1 <- arrayInt
        symbol "+"
        a2 <- arrayInt 
        return $ zipWith (+) a1 a2
    <|>
    do
        a1 <- arrayInt
        symbol "-"
        a2 <- arrayInt 
        return $ zipWith (-) a1 a2
    <|>
    do
        a1 <- arrayInt
        symbol "*"
        a2 <- arrayInt
        return $ zipWith (*) a1 a2
    <|>
    do
        a1 <- arrayInt
        symbol "/"
        a2 <- arrayInt 
        if (0 `elem` a2) then empty
        else return $ zipWith (div) a1 a2
    <|>
    --Inner product
    do
        a1 <- arrayInt
        symbol "@"
        a2 <- arrayInt 
        if (length a1 /= length a2) then empty
        else return $ [vecProd a1 a2]
    <|>
    -- Array concatenation
    do
        a1 <- arrayInt
        symbol "++"
        a2 <- arrayInt 
        return $ a1 ++ a2
    <|>
    arrayInt

arrayInt :: Parser [Int]
arrayInt =
    do 
        symbol "("
        a <- aexpArray
        symbol ")"
        return a
    <|>
    do 
        symbol "["
        symbol "]"
        return []
    <|>
    do
        symbol "["
        array <- arrayIntEntries
        symbol "]"
        return array
    <|>
    do
        i <- identifier
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_int
            then
            case var of
                Left var -> empty
                Right var -> return (read var :: [Int])
            else
                empty
    <|>
    do
        i <- identifier
        symbol "["
        rIndex <- aexp
        symbol "]"
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_arr_int
            then
            case var of
                Left var -> empty
                Right var -> return $ (read var :: [[Int]]) !! rIndex
            else
                empty 

arrayIntEntries :: Parser [Int]
arrayIntEntries =
    do  {
        number <- aexp;
        do 
            symbol ","
            a <- arrayIntEntries
            return ([number] ++ a)
        <|>
        return [number];
    }

-- Matrices of Integers
aexpMatrix :: Parser [[Int]]
aexpMatrix =
    -- Scalar product
    do
        a <- matrixInt
        symbol "*"
        t <- aexp
        return (fmap (fmap (*t)) a)
    <|>
    do
        t <- aexp
        symbol "*"
        a <- matrixInt
        return (fmap (fmap (*t)) a)
    <|>
    -- Scalar division
    do
        a <- matrixInt
        symbol "/"
        t <- aexp
        return (fmap (fmap (`div` t)) a)
    <|>
    -- Matrix Addition
    do
        a1 <- matrixInt
        symbol "+"
        a2 <- matrixInt
        return $ zipWith (zipWith (+)) a1 a2
    <|>
    -- Matrix Subtraction
    do
        a1 <- matrixInt
        symbol "-"
        a2 <- matrixInt
        return $ zipWith (zipWith (-)) a1 a2
    <|>
    -- Matrix Multiplication
    do
        a1 <- matrixInt
        symbol "@"
        a2 <- matrixInt
        return $ matMult a1 a2
    <|>
    --Matrix Horizontal Concatenation
    do
        a1 <- matrixInt
        symbol "++"
        a2 <- matrixInt
        return $ zipWith (++) a1 a2
    <|>
    --Matrix Vertical Concatenation
    do
        a1 <- matrixInt
        symbol "#"
        a2 <- matrixInt
        return $ a1 ++ a2
    <|>
    matrixInt

matrixInt :: Parser [[Int]]
matrixInt = 
    do 
        symbol "("
        a <- aexpMatrix
        symbol ")"
        return a
    <|>
    do 
        symbol "["
        symbol "["
        symbol "]"
        symbol "]"
        return [[]]
    <|>
    do
        symbol "["
        matrix <- matrixIntEntries
        symbol "]"
        return matrix
    <|>
    do
        i <- identifier
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_arr_int
            then
                case var of
                    Left var -> empty
                    Right var -> return (read var :: [[Int]])
            else
                empty
        
matrixIntEntries :: Parser [[Int]]
matrixIntEntries =
    do  {
        v1 <- arrayInt;
        do 
            symbol ","
            v2 <- matrixIntEntries
            return ([v1] ++ v2)
        <|>
        return [v1];
    }

-- Arrays of Booleans
bexpArray :: Parser [Bool]
bexpArray = 
    --Term-by-term Conjunction
    do
        a1 <- arrayBool
        symbol "&&"
        a2 <- arrayBool
        return $ zipWith (&&) a1 a2
    <|>
    -- Term-by-term Disjunction
    do
        a1 <- arrayBool
        symbol "||"
        a2 <- arrayBool
        return $ zipWith (||) a1 a2
    <|>
    -- Concatenation
    do
        a1 <- arrayBool
        symbol "++"
        a2 <- arrayBool
        return $ a1 ++ a2
    <|>
    arrayBterm
    <|>
    arrayBool

arrayBterm :: Parser [Bool]
arrayBterm =
    do  
        symbol "("
        b <- bexpArray
        symbol ")"
        return b
    <|>
    do
    -- Negation
        symbol "!"
        b <- arrayBool
        return $ fmap (not) b

arrayBool :: Parser [Bool]
arrayBool =
    do
        symbol "["
        array <- arrayBoolEntries
        symbol "]"
        return array
    <|>
    do 
        symbol "["
        symbol "]"
        return []

arrayBoolEntries :: Parser [Bool]
arrayBoolEntries =
    do  {
        bool <- bexp;
        do 
            symbol ","
            a <- arrayBoolEntries
            return ([bool] ++ a)
        <|>
        return [bool];
    }
    <|>
    do
        i <- identifier
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_bool
            then
            case var of
                Left var -> empty
                Right var -> return (read var :: [Bool])
            else
                empty
    <|>
    do
        i <- identifier
        symbol "["
        rIndex <- aexp
        symbol "]"
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_arr_bool
            then
            case var of
                Left var -> empty
                Right var -> return $ (read var :: [[Bool]]) !! rIndex
            else
                empty 

-- Matrices of Bool
bexpMatrix :: Parser [[Bool]]
bexpMatrix =
    -- Negation
    do
        a <- matrixBool
        symbol "!"
        t <- bexp
        return (fmap (fmap not) a)
    <|>
    -- Matrix Conjunction
    do
        a1 <- matrixBool
        symbol "&&"
        a2 <- matrixBool
        return $ zipWith (zipWith (&&)) a1 a2
    <|>
    -- Matrix Disjunction
    do
        a1 <- matrixBool
        symbol "||"
        a2 <- matrixBool
        return $ zipWith (zipWith (||)) a1 a2
    <|>
    --Matrix Horizontal Concatenation
    do
        a1 <- matrixBool
        symbol "++"
        a2 <- matrixBool
        return $ zipWith (++) a1 a2
    <|>
    --Matrix Vertical Concatenation
    do
        a1 <- matrixBool
        symbol "#"
        a2 <- matrixBool
        return $ a1 ++ a2
    <|>
    matrixBool

matrixBool :: Parser [[Bool]]
matrixBool = 
    do
        symbol "["
        matrix <- matrixBoolEntries
        symbol "]"
        return matrix
    <|>
    do 
        symbol "["
        symbol "["
        symbol "]"
        symbol "]"
        return [[]]

matrixBoolEntries :: Parser [[Bool]]
matrixBoolEntries =
    do  {
        v1 <- arrayBool;
        do 
            symbol ","
            v2 <- matrixBoolEntries
            return ([v1] ++ v2)
        <|>
        return [v1];
    }
    <|>
    do
        i <- identifier
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_arr_bool
            then
            case var of
                Left var -> empty
                Right var -> return (read var :: [[Bool]])
            else
                empty

--Strings
stringExp :: Parser String
stringExp =
    do
        s1 <- stringTerm
        symbol "++"
        s2 <- stringExp
        return $ s1 ++ s2
    <|> 
    stringTerm

stringTerm :: Parser String
stringTerm = 
    do
        i <- identifier
        (var, vtype) <- readFullVariable i
        if (vtype == "string")
            then 
                case var of
                Left var -> empty
                Right var -> return var
        else 
            empty
    <|>
    do
        i <- identifier
        symbol "["
        index <- aexp
        symbol "]"
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_string
            then
            case var of
                Left var -> empty
                Right var -> return $ (read var :: [String]) !! index
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
        if vtype == t_arr_arr_string
            then
            case var of
                Left var -> empty
                Right var -> return $ ((read var :: [[String]]) !! rIndex) !! cIndex
            else
                empty 
    <|>
    string

--Arrays of Strings
stringExpArray :: Parser [String]
stringExpArray =
    -- Concatenation
    do
        a1 <- arrayString
        symbol "++"
        a2 <- arrayString
        return $ a1 ++ a2
    <|>
    arrayString
    
arrayString :: Parser [String]
arrayString =
    do
        symbol "["
        array <- arrayStringEntries
        symbol "]"
        return array
    <|>
    do 
        symbol "["
        symbol "]"
        return []


arrayStringEntries :: Parser [String]
arrayStringEntries =
    do  {
        stringVal <- stringExp;
        do 
            symbol ","
            a <- arrayStringEntries
            return ([stringVal] ++ a)
        <|>
        return [stringVal];
    }   
    <|>
    do
        i <- identifier
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_string
            then
            case var of
                Left var -> empty
                Right var -> return (read var :: [String])
            else
                empty
    <|>
    do
        i <- identifier
        symbol "["
        rIndex <- aexp
        symbol "]"
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_arr_string
            then
            case var of
                Left var -> empty
                Right var -> return $ (read var :: [[String]]) !! rIndex
            else
                empty 

stringExpMatrix :: Parser [[String]]
stringExpMatrix =
    --Matrix Horizontal Concatenation
    do
        a1 <- matrixString
        symbol "++"
        a2 <- matrixString
        return $ zipWith (++) a1 a2
    <|>
    --Matrix Vertical Concatenation
    do
        a1 <- matrixString
        symbol "#"
        a2 <- matrixString
        return $ a1 ++ a2
    <|>
    matrixString

matrixString:: Parser [[String]]
matrixString = 
    do
        symbol "["
        matrix <- matrixStringEntries
        symbol "]"
        return matrix
    <|>
    do 
        symbol "["
        symbol "["
        symbol "]"
        symbol "]"
        return [[]]
    
matrixStringEntries :: Parser [[String]]
matrixStringEntries =
    do  {
        v1 <- arrayString;
        do 
            symbol ","
            v2 <- matrixStringEntries
            return ([v1] ++ v2)
        <|>
        return [v1];
    }
    <|>
    do
        i <- identifier
        (var, vtype) <- readFullVariable i
        if vtype == t_arr_arr_string
            then
            case var of
                Left var -> empty
                Right var -> return (read var :: [[String]])
            else
                empty

--Matrix operations
matTran :: [[Int]] -> [[Int]]
matTran [[]] = [[]]
matTran m = if length (head m) > 1
    then (map head m) : (matTran (map tail m))
    else [map head m]
    

matMult :: [[Int]] -> [[Int]] -> [[Int]]
matMult m1 m2 = matMult' m1 (matTran m2)    

matMult' :: [[Int]] -> [[Int]] -> [[Int]]
matMult' _ [] = [[]] 
matMult' [] _ = [[]] 
matMult' (r:m1) (m2) = if length m1 > 0
    then [matMultRow (r:m1) m2] ++ (matMult' m1 m2)
    else [matMultRow (r:m1) m2]

matMultRow :: [[Int]] -> [[Int]] -> [Int]
matMultRow [] _ = []
matMultRow _ [] = [] 
matMultRow (row:m1) (col:m2) = if length m2 > 0
    then [vecProd row col] ++ (matMultRow (row:m1) m2)
    else [vecProd row col]

vecProd :: [Int] -> [Int] -> Int
vecProd a1 a2 = sum (zipWith (*) a1 a2)