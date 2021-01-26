module Parser.Command where
import Control.Applicative
import qualified Debug.Trace as DBG
import Parser.Core
import Parser.Aexp(aexp)
import Parser.Bexp(bexp)
import Parser.Array
import Parser.EnvironmentManager
import Environment(Variable(..))
import qualified Parser.ReadOnlyParser.CommandROP as CR

program :: Parser String
program = 
    do
        command
        program
    <|>
    command

command :: Parser String
command =
    assignment
    <|> 
    skip
    <|>
    ifThenElse
    <|>
    while

assignment :: Parser String
assignment = 
    --Aexp
    do 
        varName <- identifier
        symbol "="
        aval <- aexp
        --DBG.traceM(show aval)
        symbol ";"
        updateEnv Variable {name=varName, vtype=t_int, value = Left aval}
    <|>
    --Bexp
    do
        varName <- identifier
        symbol "="
        bval <- bexp
       -- DBG.traceM(show bval)
        symbol ";"
        if bval
        then updateEnv Variable {name=varName, vtype=t_bool, value = Left 0}
        else updateEnv Variable {name=varName, vtype=t_bool, value = Left 1}
    <|>
    -- String
    do
        varName <- identifier
        symbol "="
        stringVal <- stringExp
        --DBG.traceM(show stringVal)
        symbol ";" 
        updateEnv Variable {name=varName, vtype=t_string, value = Right stringVal}
    <|>
    --Int Array
    do
        varName <- identifier
        symbol "="
        arrayVal <- aexpArray
        symbol ";" 
        updateEnv Variable {name=varName, vtype=t_arr_int , value = Right (show arrayVal)}
    <|>
    do
        arrayName <- identifier
        symbol "["
        index <- nat
        symbol "]"
        symbol "="
        newValue <- aexp
        symbol ";" 
        updateArray arrayName index (show newValue)
    <|>
    --Int Matrices / Array of Arrays
    do
        varName <- identifier
        symbol "="
        arrayVal <- aexpMatrix
        symbol ";" 
        updateEnv Variable {name=varName, vtype=t_arr_arr_int , value = Right (show arrayVal)}
    <|>
    do
        arrayName <- identifier
        symbol "["
        index <- nat
        symbol "]"
        symbol "="
        newValue <- aexpArray
        symbol ";" 
        updateArray arrayName index (show newValue)
    <|>
    do
        matrixName <- identifier
        symbol "["
        indexR <- nat
        symbol "]"
        symbol "["
        indexC <- nat
        symbol "]"
        symbol "="
        newValue <- aexp
        symbol ";" 
        updateMatrixEntry matrixName indexR indexC (show newValue)
    <|>
    --Bool Array
    do
        varName <- identifier
        symbol "="
        arrayVal <- bexpArray
        symbol ";" 
        updateEnv Variable {name=varName, vtype=t_arr_bool , value = Right (show arrayVal)}
    <|>
    do
        arrayName <- identifier
        symbol "["
        index <- nat
        symbol "]"
        symbol "="
        newValue <- bexp
        symbol ";" 
        updateArray arrayName index (show newValue)
    <|>
    --Bool Matrices / Array of Arrays
    do
        varName <- identifier
        symbol "="
        arrayVal <- bexpMatrix
        symbol ";" 
        updateEnv Variable {name=varName, vtype=t_arr_arr_bool , value = Right (show arrayVal)}
    <|>
    do
        arrayName <- identifier
        symbol "["
        index <- nat
        symbol "]"
        symbol "="
        newValue <- bexpArray
        symbol ";" 
        updateArray arrayName index (show newValue)
    <|>
    do
        matrixName <- identifier
        symbol "["
        indexR <- nat
        symbol "]"
        symbol "["
        indexC <- nat
        symbol "]"
        symbol "="
        newValue <- bexp
        symbol ";" 
        updateMatrixEntry matrixName indexR indexC (show newValue)
    <|>
    --String Array
    do
        varName <- identifier
        symbol "="
        arrayVal <- stringExpArray
        symbol ";" 
        updateEnv Variable {name=varName, vtype=t_arr_string, value = Right (show arrayVal)}
    <|>
    do
        arrayName <- identifier
        symbol "["
        index <- nat
        symbol "]"
        symbol "="
        newValue <- stringExp
        symbol ";" 
        updateArray arrayName index newValue  
    <|>  
    --String Matrices / Array of Arrays
    do
        varName <- identifier
        symbol "="
        arrayVal <- stringExpMatrix
        symbol ";" 
        updateEnv Variable {name=varName, vtype=t_arr_arr_string , value = Right (show arrayVal)}
    <|>
    do
        arrayName <- identifier
        symbol "["
        index <- nat
        symbol "]"
        symbol "="
        newValue <- stringExpArray
        symbol ";" 
        updateArray arrayName index (show newValue)
    <|>
    do
        matrixName <- identifier
        symbol "["
        indexR <- nat
        symbol "]"
        symbol "["
        indexC <- nat
        symbol "]"
        symbol "="
        newValue <- stringExp
        symbol ";" 
        updateMatrixEntry matrixName indexR indexC newValue

skip :: Parser String
skip = 
    do
        symbol "skip"
        symbol ";"

ifThenElse :: Parser String
ifThenElse =
    do
        symbol "if"
        symbol "("
        b <- bexp
        symbol ")"
        symbol "{"
        if b
            then do {
                program;
                symbol "}";
                do {
                    symbol "else";
                    symbol "{";
                    CR.program;
                    symbol "}";
                    return "";
                } 
                <|>
                return ""
            }
        else 
            do {
            CR.program;
            symbol "}";
            do
                symbol "else"
                symbol "{"
                program
                symbol "}"
                return ""
            <|>
            return ""
        }

while :: Parser String
while = 
    do {
        w <- CR.while;
        CR.repeatWhileString w;
        symbol "while";
        symbol "(";
        condition <- bexp;
        symbol ")";
        symbol "{";
        if condition 
            then do {
                program;
                symbol "}";
                CR.repeatWhileString w;
                while;
            }
        else
            do {
                CR.program;
                symbol "}";
                return "";
            }
    }