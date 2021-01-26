module Environment (
    Variable(..), Env, modifyEnv, searchVariable
)
where 

data Variable = Variable {
    name :: String,
    vtype :: String,
    value :: Either Int String 
} deriving Eq

instance Show Variable where
    show var = (name var) ++  " = " ++ varValue ++ " (" ++ (vtype var) ++ ")\n"
            where varValue = case (value var) of
                    Left val  -> show val
                    Right val -> val

type Env = [Variable]

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) 
                        then [newVar] ++ xs
                        else [x] ++ modifyEnv xs newVar

-- Searches for the value of a variable stored in the Env given the name
searchVariable :: Env -> String -> Maybe ((Either Int String), String)
searchVariable    []       _    = Nothing
searchVariable (x:xs) queryname = if (name x) == queryname
    then Just (value x, vtype x)
    else searchVariable xs queryname