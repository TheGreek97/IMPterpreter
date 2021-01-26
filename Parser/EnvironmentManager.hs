module Parser.EnvironmentManager
where
import Text.Read
import Environment
import Parser.Core (Parser (..))
import qualified Debug.Trace as DBG

-- Updates the environment with a variable
-- If the variable is new (not declared before), it will be added to the environment,
-- if it already exists, its value will be overwritten
updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of 
                    xs -> Just ((modifyEnv env var), "", xs))

-- Returns the value of a variable given the name
readVariable :: String -> Parser (Either Int String)
readVariable name = P (\env input-> case searchVariable env name of
    Nothing -> Nothing
    Just (value, _) -> Just (env, value, input))

-- Returns the value and the type of a variable given the 
readFullVariable :: String -> Parser (Either Int String, String)
readFullVariable name = P (\env input-> case searchVariable env name of
    Nothing -> Nothing
    Just (value, vtype) -> Just (env, (value, vtype), input))

updateArray :: String -> Int -> String -> Parser String
updateArray arrName index newElement = 
    P (\env input-> case searchVariable env arrName of
        Nothing -> Nothing
        Just (value, arrType) -> case value of
            Left _ -> Nothing
            Right value ->
                case arrType of
                "[int]" -> 
                    Just ( 
                        let newArray = replaceInArray ( read value :: [Int]) index (read newElement :: Int)          
                        in (modifyEnv env Variable{ name = arrName, vtype = arrType, value = Right $ show newArray}), "", input
                        )
                "[[int]]" -> 
                    Just (
                        let newArray = replaceInArray ( read value :: [[Int]]) index (read newElement :: [Int])
                        in (modifyEnv env Variable{ name = arrName, vtype = arrType, value = Right $ show newArray}), "", input
                        )
                "[bool]" ->
                    Just ( 
                        let newArray = replaceInArray ( read value :: [Bool]) index (read newElement :: Bool)
                        in (modifyEnv env Variable{ name = arrName, vtype = arrType, value = Right $ show newArray}), "", input
                    )
                "[[bool]]" ->
                    Just ( 
                        let newArray = replaceInArray ( read value :: [[Bool]]) index (read newElement :: [Bool])
                        in (modifyEnv env Variable{ name = arrName, vtype = arrType, value = Right $ show newArray}), "", input
                    )
                "[string]" -> 
                    Just ( 
                        let newArray = replaceInArray ( read value :: [String]) index newElement
                        in (modifyEnv env Variable{ name = arrName, vtype = arrType, value = Right $ show newArray}), "", input
                        )
                "[[string]]" -> 
                    Just ( 
                        let newArray = replaceInArray ( read value :: [[String]]) index (read newElement :: [String])
                        in (modifyEnv env Variable{ name = arrName, vtype = arrType, value = Right $ show newArray}), "", input
                        )
                otherwise -> Nothing
    )

updateMatrixEntry :: String -> Int -> Int -> String -> Parser String
updateMatrixEntry matName rowIndex colIndex newElement = 
    P (\env input-> case searchVariable env matName of
        Nothing -> Nothing
        Just (value, matType) -> case value of
            Left _ -> Nothing
            Right value -> case matType of
                "[[int]]" -> 
                    Just (
                        let newRow = replaceInArray ((read value :: [[Int]])!!rowIndex) colIndex (read newElement :: Int)
                            newMatrix = replaceInArray ( read value :: [[Int]]) rowIndex newRow
                        in (modifyEnv env Variable{ name = matName, vtype = matType, value = Right $ show newMatrix}), "", input
                        )
                otherwise -> Nothing
    )

replaceInArray :: [a] -> Int -> a -> [a]
replaceInArray array index newEntry= 
    let arrayStart = take index array 
        arrayEnd = drop (index+1) array
    in arrayStart ++ [newEntry] ++ arrayEnd