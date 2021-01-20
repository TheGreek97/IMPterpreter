module Parser.EnvironmentManager
where
import Environment
import Parser.Core (Parser (..))

-- Updates the environment with a variable
-- If the variable is new (not declared before), it will be added to the environment,
-- if it already exists, its value will be overwritten

updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of 
                    xs -> Just ((modifyEnv env var), "", xs))

-- Returns the value of a variable given the name
readVariable :: String -> Parser Int
readVariable name = P (\env input-> case searchVariable env name of
    [] -> Nothing
    [(value, _)] -> Just (env, value, input))

-- Returns the type of a variable given the name
readVariableType :: String -> Parser (Int, String)
readVariableType name = P (\env input-> case searchVariable env name of
    [] -> Nothing
    [(value, vtype)] -> Just (env, (value, vtype), input))