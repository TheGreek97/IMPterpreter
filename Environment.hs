module Environment (
    Variable(..), Env, modifyEnv, searchVariable
)
where 

data Variable = Variable {
    name :: String,
    vtype :: String,
    value :: Int 
} deriving Show

type Env = [Variable]

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) 
                        then [newVar] ++ xs
                        else [x] ++ modifyEnv xs newVar

-- Searches for the value of a variable stored in the Env given the name
searchVariable :: Env -> String -> [(Int, String)]
searchVariable []     _         = []
searchVariable (x:xs) queryname = if (name x) == queryname
    then [(value x, vtype x)]
    else searchVariable xs queryname