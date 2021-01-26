module Parser.Array where
import Control.Applicative
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
        symbol "["
        array <- arrayInt
        symbol "]"
        return array
    <|>
    do 
        symbol "["
        symbol "]"
        return []
    <|>
    do  {
        number <- aexp;
        do 
            symbol ","
            a <- arrayInt
            return ([number] ++ a)
        <|>
        return [number];
    }
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
        symbol "["
        matrix <- matrixInt
        symbol "]"
        return matrix
    <|>
    do 
        symbol "["
        symbol "]"
        return [[]]
    <|>
    do  {
        symbol "[";
        v1 <- arrayInt;
        symbol "]";
        do 
            symbol ","
            v2 <- matrixInt
            return ([v1] ++ v2)
        <|>
        return [v1];
    }
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

-- Arrays of Booleans
bexpArray :: Parser [Bool]
bexpArray = 
    --Conjunction
    do
        a1 <- arrayBool
        symbol "&&"
        a2 <- arrayBool
        return $ zipWith (&&) a1 a2
    <|>
    -- Disjunction
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
        symbol "!"
        b <- arrayBool
        return $ fmap (not) b

arrayBool :: Parser [Bool]
arrayBool =
    do
        symbol "["
        array <- arrayBool
        symbol "]"
        return array
    <|>
    do 
        symbol "["
        symbol "]"
        return []
    <|>
    do  {
        bool <- bexp;
        do 
            symbol ","
            a <- arrayBool
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
        matrix <- matrixBool
        symbol "]"
        return matrix
    <|>
    do 
        symbol "["
        symbol "]"
        return [[]]
    <|>
    do  {
        symbol "[";
        v1 <- arrayBool;
        symbol "]";
        do 
            symbol ","
            v2 <- matrixBool
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
        array <- arrayString
        symbol "]"
        return array
    <|>
    do 
        symbol "["
        symbol "]"
        return []
    <|>
    do  {
        stringVal <- stringExp;
        do 
            symbol ","
            a <- arrayString
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
        matrix <- matrixString
        symbol "]"
        return matrix
    <|>
    do 
        symbol "["
        symbol "]"
        return [[]]
    <|>
    do  {
        symbol "[";
        v1 <- arrayString;
        symbol "]";
        do 
            symbol ","
            v2 <- matrixString
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