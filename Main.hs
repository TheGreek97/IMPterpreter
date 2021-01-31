module Main where
import System.IO
import Parser (evaluate)
import Environment

main :: IO String
main = do
    printLogo
    repeatProgram []
        
repeatProgram :: Env -> IO String
repeatProgram e = do
    prog <- getInputProgram
    if (prog == ":q" || prog == ":exit")
    then return "Goodbye!"
    else do
        case evaluate prog e of
            Left err ->
                do
                    putStrLn err
                    repeatProgram e
            Right result ->
                do
                    putStrLn $ show result
                    repeatProgram result



getInputProgram :: IO String
getInputProgram = do
    putStr   ("IMP> ");
    hFlush stdout;
    getLine;

printLogo :: IO String
printLogo = do
    --putStr "\ESC[2J";
    putStrLn ("   _____          ___ _                           _            ")
    putStrLn ("  \\_   \\/\\/\\    / _ \\ |_ ___ _ __ _ __  _ __ ___| |_ ___ _ __ ")
    putStrLn ("   / /\\/    \\  / /_)/ __/ _ \\ '__| '_ \\| '__/ _ \\ __/ _ \\ '__|")
    putStrLn (" \\/ /_/ /\\/\\ \\/ ___/| ||  __/ |  | |_) | | |  __/ ||  __/ |   ")
    putStrLn ("\\____/\\/    \\/\\/     \\__\\___|_|  | .__/|_|  \\___|\\__\\___|_|   ")
    putStrLn ("                                 |_|                          ")
    putStrLn ("       - Developed by Francesco Greco -        ")
    putStrLn ("Welcome to IMPterpter, the interpreter for IMP!")
    putStrLn ("")
    putStrLn ("Please type in the code to interpret")
    putStrLn ("If you want to exit, just type \":q\" or \":exit\", or simply enter CTRL+Z")
    return ""
