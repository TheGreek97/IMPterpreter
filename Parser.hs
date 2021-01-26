module Parser (evaluate)
where 
import Environment
import Parser.Core
import Parser.EnvironmentManager
import Parser.Aexp
import Parser.Bexp
import Parser.Command
import Parser.Array

evaluate :: String -> Env -> Either String Env
evaluate p inEnv = case parse program inEnv p of
    Nothing               -> Left "Error in the evaluation of the program.\nPlease check your syntax."
    Just (e, _, "")       -> Right e
    Just (e, _, residual) -> Left $ "Error in the evaluation of the program.\n"++
        "Please check your syntax.\nThe error occured around the statement:\n \""++
        (take 300 residual) ++"\""