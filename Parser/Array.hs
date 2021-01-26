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

-- Matrices of Integers
aexpMatrix :: Parser [[Int]]
aexpMatrix =
    -- Scalar product
    do
        a <- arrArrInt
        symbol "*"
        t <- aexp
        return (fmap (fmap (*t)) a)
    <|>
    -- Scalar division
    do
        a <- arrArrInt
        symbol "/"
        t <- aexp
        return (fmap (fmap (`div` t)) a)
    <|>
    -- Matrix Addition
    do
        a1 <- arrArrInt
        symbol "+"
        a2 <- arrArrInt
        return $ zipWith (zipWith (+)) a1 a2
    <|>
    -- Matrix Subtraction
    do
        a1 <- arrArrInt
        symbol "-"
        a2 <- arrArrInt
        return $ zipWith (zipWith (-)) a1 a2
    <|>
    -- Matrix Multiplication
    do
        a1 <- arrArrInt
        symbol "@"
        a2 <- arrArrInt
        return $ matMult a1 a2
    <|>
    arrArrInt

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

arrArrInt :: Parser [[Int]]
arrArrInt = 
    do
        symbol "["
        matrix <- arrArrInt
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
            v2 <- arrArrInt
            return ([v1] ++ v2)
        <|>
        return [v1];
    }

-- Arrays of Booleans
bexpArray :: Parser [Bool]
bexpArray = 
    do
        a1 <- arrayBool
        symbol "||"
        a2 <- arrayBool
        return $ zipWith (||) a1 a2
    <|>
    do
        a1 <- arrayBool
        symbol "&&"
        a2 <- arrayBool
        return $ zipWith (&&) a1 a2

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

parseArrArrBool :: Parser [[Bool]]
parseArrArrBool = 
    do
        symbol "["
        matrix <- parseArrArrBool
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
            v2 <- parseArrArrBool
            return ([v1] ++ v2)
        <|>
        return [v1];
    }

--Arrays of Strings
parseArrString :: Parser [String]
parseArrString =
    do
        symbol "["
        array <- parseArrString
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
            a <- parseArrString
            return ([stringVal] ++ a)
        <|>
        return [stringVal];
    }

parseArrArrString:: Parser [[String]]
parseArrArrString = 
    do
        symbol "["
        matrix <- parseArrArrString
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
        v1 <- parseArrString;
        symbol "]";
        do 
            symbol ","
            v2 <- parseArrArrString
            return ([v1] ++ v2)
        <|>
        return [v1];
    }