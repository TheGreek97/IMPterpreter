module Parser.EnvironmentManager
where
import Text.Read
import Environment
import Parser.Core (Parser (..))

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