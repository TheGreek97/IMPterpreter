module Parser.Command where
import Control.Applicative
import qualified Debug.Trace as DBG
import Parser.Core
import Parser.Aexp(aexp)
import Parser.Bexp(bexp)
import Parser.EnvironmentManager(updateEnv)
import Environment(Variable(..))
import qualified Parser.ReadOnlyParser.CommandROP as ROP

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
    do 
        varName <- identifier
        symbol "="
        do
            aval <- aexp
            --DBG.traceM(show aval)
            symbol ";"
            updateEnv Variable {name=varName, vtype="int", value = aval}
    <|>
    do
        varName <- identifier
        symbol "="
        bval <- bexp
       -- DBG.traceM(show bval)
        symbol ";"
        if bval
        then updateEnv Variable {name=varName, vtype="bool", value = 0}
        else updateEnv Variable {name=varName, vtype="bool", value = 1}

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
                    ROP.program;
                    symbol "}";
                    return "";
                } 
                <|>
                return ""
            }
        else 
            do {
            ROP.program;
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
        w <- ROP.while;
        ROP.repeatWhileString w;
        symbol "while";
        symbol "(";
        condition <- bexp;
        symbol ")";
        symbol "{";
        if condition 
            then do {
                program;
                symbol "}";
                ROP.repeatWhileString w;
                while;
            }
        else
            do {
                ROP.program;
                symbol "}";
                return "";
            }
    }